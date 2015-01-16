%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: mercury_mongoose.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Thu 18 Dec 17:38:25 CST 2014
% Stability: low
%----------------------------------------------------------------------------%
% API to access the lightwight mongoose HTTP(S) web-server.
% The open-source (GPLv2) version is used for this library, i.e. that
% means that this library is also licensed under the GPLv2.
% c.f.: https://github.com/cesanta/mongoose
%       http://en.wikipedia.org/wiki/Mongoose_(web_server)
%
% TODO: check if http://www.microhowto.info/howto/
%                serve_web_pages_using_an_embedded_http_server_in_java.html
%       is compatible with the mongoose api.
%----------------------------------------------------------------------------%

:- module mercury_mongoose.

:- interface.

:- include_module mercury_mongoose.path_util.
:- import_module mercury_mongoose.path_util.

:- import_module bool.
:- import_module io.
:- import_module list.

%----------------------------------------------------------------------------%

:- type server.

:- type connection.

:- type socket_address
    --->    socket_address(
                socket_ip   :: ip_address,
                socket_port :: port
            ).

:- type ip_address == string.   % TODO: IP address ADT

:- type handler_func == (func(connection, event, io, io) = handler_result).
:- inst handler_func == (func(in, in, di, uo) = (out) is det).

:- type handler_result
    --->    true
    ;       false
    ;       more.

:- type (event)
    --->    poll
    ;       connect
    ;       auth
    ;       request
    ;       reply
    ;       recv
    ;       close
    ;       ws_handshake
    ;       ws_connect
    ;       http_error.

:- type websocket_opcode
    --->    continuation
    ;       text
    ;       binary
    ;       connection_close
    ;       ping
    ;       pong.

:- type option
    --->    listening_port
    ;       document_root.

:- inst listening_port ---> listening_port.

:- inst document_root ---> document_root.

:- type option_value
    --->    port(port)
    ;       path(string).

:- inst port ---> port(ground).

:- inst path ---> path(ground).

:- type port == int.

:- type http_status == int.

:- type http_version == string.

%----------------------------------------------------------------------------%
%
% Server management predicates.
%

    % create(Server, Handler, !IO):
    %
:- pred create(server::uo, handler_func::in(handler_func),
    io::di, io::uo) is det.

    % destroy(Server, !IO):
    %
:- pred destroy(server::in, io::di, io::uo) is det.

    % poll(Server, Loop, Milliseconds, !IO):
    % TODO: Create loop type to stop on signals, etc.
    %
:- pred poll(server::in, bool::in, int::in, io::di, io::uo) is det.

    % set_option(Server, Option, Value, !IO):
    %
:- pred set_option(server, option, option_value, io, io).
:- mode set_option(in, in(listening_port), in(port), di, uo) is det.
:- mode set_option(in, in(document_root),  in(path), di, uo) is det.

    % get_option(Server, Option, Value, !IO):
    %
:- pred get_option(server, option, option_value, io, io).
:- mode get_option(in, in(listening_port), out(port), di, uo) is det.

%----------------------------------------------------------------------------%
%
% Connection properties and I/O.
%

:- func (connection::in) ^ server_handler =
    (handler_func::out(handler_func)) is det.

:- func connection ^ request_path = decoded_uri_path.

:- func connection ^ remote_address = socket_address.

:- func connection ^ remote_ip = ip_address.

:- func connection ^ remote_port = port.

:- func connection ^ local_address = socket_address.

:- func connection ^ local_ip = ip_address.

:- func connection ^ local_port = port.

:- func connection ^ http_version = http_version.

    % is_websocket(Connection):
    %
:- pred is_websocket(connection::in) is semidet.

%----------------------------------------------------------------------------%

    % send_string_data(Connection, String, BytesWritten, !IO):
    %
:- pred send_string_data(connection::in, string::in, int::out,
    io::di, io::uo) is det.

    % printf_data(Connection, FmtString, Params, BytesWritten, !IO):
    %
:- pred printf_data(connection::in, string::in, list(poly_type)::in,
    int::out, io::di, io::uo) is det.

    % get_status(Connection, Status, !IO):
    %
:- pred get_status(connection::in, http_status::out, io::di, io::uo) is det.

    % send_status(Connection, Status, !IO):
    %
:- pred send_status(connection::in, http_status::in, io::di, io::uo) is det.

    % send_header(Connection, Header, Value, !IO):
    %
:- pred send_header(connection::in, string::in, string::in, io::di, io::uo)
    is det.

    % send_file(Connection, FilePath, !IO):
    % calls `send_file/5' without a header.
    %
:- pred send_file(connection::in, string::in, io::di, io::uo) is det.

    % send_file(Connection, FilePath, Header, !IO):
    %
:- pred send_file(connection::in, string::in, string::in, io::di, io::uo)
    is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module int. % for int_to_string/1, det_to_int/1
:- import_module string.

%----------------------------------------------------------------------------%
%
% Implemtation of server management functions and types in C
%

    % `include_file()' is preferred over `#include', since then linking
    % to the library does not require the presence of the API header
    %
:- pragma foreign_decl("C", include_file("mongoose.h")).

:- pragma foreign_type("C", server,
    "struct mg_server *", [can_pass_as_mercury_type]).

:- pragma foreign_type("C", connection,
    "struct mg_connection *", [can_pass_as_mercury_type]).

:- pragma foreign_enum("C", handler_result/0,
    [
        true  - "MG_TRUE",
        false - "MG_FALSE",
        more  - "MG_MORE"
    ]).

:- pragma foreign_enum("C", (event)/0,
    [
        poll    - "MG_POLL",
        connect - "MG_CONNECT",
        auth    - "MG_AUTH",
        request - "MG_REQUEST",
        reply   - "MG_REPLY",
        recv    - "MG_RECV",
        close   - "MG_CLOSE",
        ws_handshake - "MG_WS_HANDSHAKE",
        ws_connect   - "MG_WS_CONNECT",
        http_error   - "MG_HTTP_ERROR"
    ]).

:- pragma foreign_proc("C",
    create(Server::uo, Handler::in(handler_func), _IO0::di, _IO::uo),
    [promise_pure, may_call_mercury],
"
    Server = mg_create_server((void*)Handler, (mg_handler_t)mercury_handler);
").

:- pragma foreign_proc("C",
    destroy(Server::in, _IO0::di, _IO::uo), [promise_pure],
"
    mg_destroy_server(&Server);
").

:- pragma foreign_proc("C",
    poll(Server::in, Loop::in, Milliseconds::in, _IO0::di, _IO::uo),
    [promise_pure, may_call_mercury],
"
    do {
        mg_poll_server(Server, Milliseconds);
    } while (Loop);
").

set_option(Server, listening_port, port(Port), !IO) :-
    set_option_string(Server, "listening_port", int_to_string(Port), !IO).

set_option(Server, document_root, path(Root), !IO) :-
    set_option_string(Server, "document_root", Root, !IO).

:- pred set_option_string(server::in, string::in, string::in, io::di, io::uo)
    is det.

:- pragma foreign_proc("C",
    set_option_string(Server::in, Option::in, Value::in, _IO0::di, _IO::uo),
        [promise_pure],
"
    mg_set_option(Server, Option, Value);
").

get_option(Server, listening_port, port(det_to_int(Port)), !IO) :-
    get_option_string(Server, "listening_port", Port, !IO).

:- pred get_option_string(server::in, string::in, string::out, io::di, io::uo)
    is det.

:- pragma foreign_proc("C",
    get_option_string(Server::in, Option::in, Value::out, _IO0::di, _IO::uo),
        [promise_pure],
"
    /* NOTE: Const cast */
    Value = (MR_String)((MR_ConstString)mg_get_option(Server, Option));
").

%----------------------------------------------------------------------------%
%
% Enable the server to call the HTTP handler written in Mercury.
% XXX: Not sure if this is legal Mercury.
%

:- func mercury_handler `with_type` handler_func `with_inst` handler_func.

:- pragma foreign_export("C", mercury_handler(in, in, di, uo) = out,
    "mercury_handler").

mercury_handler(Connection, Event, !IO) = Result :-
    Result = (Connection ^ server_handler)(Connection, Event, !.IO, !:IO).

%----------------------------------------------------------------------------%
%
% Implementation of connection properties and I/O.
%

:- pragma foreign_proc("C",
    server_handler(Connection::in) = (Handler::out(handler_func)),
    [promise_pure, will_not_call_mercury],
"
    Handler = (MR_Word)(Connection->server_param);
").

:- pragma foreign_proc("C",
    request_path(Connection::in) = (Uri::out),
    [promise_pure, will_not_call_mercury],
"
    Uri = (MR_String)(Connection->uri);
").

:- pragma foreign_proc("C", http_version(Connection::in) = (Version::out),
    [promise_pure, will_not_call_mercury],
"
    Version = (MR_String)(Connection->http_version);
").

:- pragma foreign_proc("C", is_websocket(Connection::in),
    [promise_pure, will_not_call_mercury],
"
    SUCCESS_INDICATOR = Connection->is_websocket ? MR_TRUE : MR_FALSE;
").

%----------------------------------------------------------------------------%

Connection ^ remote_address =
    socket_address(Connection ^ remote_ip, Connection ^ remote_port).

:- pragma foreign_proc("C", remote_ip(Connection::in) = (IP::out),
    [promise_pure, will_not_call_mercury],
"
    /* NOTE: Const cast */
    IP = (MR_String)((MR_ConstString)(Connection->remote_ip));
").

:- pragma foreign_proc("C", remote_port(Connection::in) = (Port::out),
    [promise_pure, will_not_call_mercury],
"
    Port = (MR_Integer)(Connection->remote_port);
").

%----------------------------------------------------------------------------%

Connection ^ local_address =
    socket_address(Connection ^ local_ip, Connection ^ local_port).

:- pragma foreign_proc("C", local_ip(Connection::in) = (IP::out),
    [promise_pure, will_not_call_mercury],
"
    /* NOTE: Const cast */
    IP = (MR_String)((MR_ConstString)(Connection->local_ip));
").

:- pragma foreign_proc("C", local_port(Connection::in) = (Port::out),
    [promise_pure, will_not_call_mercury],
"
    Port = (MR_Integer)(Connection->local_port);
").

%----------------------------------------------------------------------------%

:- pragma foreign_decl("C", local, "#include <string.h>").

:- pragma foreign_proc("C",
    send_string_data(Connection::in, String::in, BytesWritten::out,
        _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    BytesWritten = mg_send_data(Connection, String, strlen(String));
").

printf_data(Connection, FmtString, Params, BytesWritten, !IO) :-
    send_string_data(Connection,
        string.format(FmtString, Params),
        BytesWritten, !IO).

:- pragma foreign_proc("C",
    get_status(Connection::in, Status::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    Status = Connection->status_code;
").

:- pragma foreign_proc("C",
    send_status(Connection::in, Status::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    mg_send_status(Connection, Status);
").

:- pragma foreign_proc("C",
    send_header(Connection::in, Header::in, Value::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    mg_send_header(Connection, Header, Value);
").

:- pragma foreign_proc("C",
    send_file(Connection::in, FilePath::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    mg_send_file(Connection, FilePath, NULL);
").

:- pragma foreign_proc("C",
    send_file(Connection::in, FilePath::in, Header::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    mg_send_file(Connection, FilePath, Header);
").

%----------------------------------------------------------------------------%
:- end_module mercury_mongoose.
%----------------------------------------------------------------------------%
