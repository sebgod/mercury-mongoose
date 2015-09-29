%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: mercury_mongoose.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet@outlook.com>
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

:- type manager.

:- type connection.

:- type socket_address
    --->    socket_address(
                socket_ip   :: ip_address,
                socket_port :: port
            ).

:- type ip_address == string.   % TODO: IP address ADT

:- type event_handler_pred == pred(connection, event, io, io).
:- inst event_handler_pred == (pred(in, in, di, uo) is det).

:- type (event)
    --->    poll
    ;       connect
    ;       request
    ;       reply
    ;       recv
    ;       close
    .

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
% Mongoose public interface.
%

    % manager_init(Manager, Handler, !IO):
    %
:- pred manager_init(manager::uo, io::di, io::uo) is det.

    % bind(Manager, Address, Handler, Connection, !IO):
    %
:- pred bind(manager::in, string::in,
        event_handler_pred::in(event_handler_pred),
        connection::uo, io::di, io::uo) is det.

    % poll(Server, Loop, Milliseconds, !IO):
    % TODO: Create loop type to stop on signals, etc.
    %
:- pred poll(manager::in, bool::in, int::in, io::di, io::uo) is det.

%----------------------------------------------------------------------------%
%
% Connection properties and I/O.
%

:- func (connection::in) ^ event_handler =
    (event_handler_pred::out(event_handler_pred)) is det.

%:- func connection ^ request_path = decoded_uri_path.

%:- func connection ^ remote_address = socket_address.

%:- func connection ^ remote_ip = ip_address.

%:- func connection ^ remote_port = port.

%:- func connection ^ local_address = socket_address.

%:- func connection ^ local_ip = ip_address.

%:- func connection ^ local_port = port.

%:- func connection ^ http_version = http_version.

    % is_websocket(Connection):
    %
%:- pred is_websocket(connection::in) is semidet.

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
%:- pred get_status(connection::in, http_status::out, io::di, io::uo) is det.

    % send_status(Connection, Status, !IO):
    %
%:- pred send_status(connection::in, http_status::in, io::di, io::uo) is det.

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
% Implemtation of the Mongoose public API in C
%

    % `include_file()' is preferred over `#include', since then linking
    % to the library does not require the presence of the API header
    %
:- pragma foreign_decl("C", include_file("mongoose.h")).

:- pragma foreign_type("C", manager,
    "struct mg_mgr *", [can_pass_as_mercury_type]).

:- pragma foreign_type("C", connection,
    "struct mg_connection *", [can_pass_as_mercury_type]).

:- pragma foreign_enum("C", (event)/0,
    [
        poll    - "MG_EV_POLL",
        connect - "MG_EV_CONNECT",
        request - "MG_EV_HTTP_REQUEST",
        reply   - "MG_EV_HTTP_REPLY",
        recv    - "MG_EV_RECV",
        close   - "MG_EV_CLOSE"
    ]).

:- pragma foreign_decl("C",
"
extern void
MMG_mgr_finalize(void *mgr, void *user_data);
").

:- pragma foreign_proc("C",
    manager_init(Manager::uo, _IO0::di, _IO::uo),
    [promise_pure, may_call_mercury],
"
    Manager = MR_GC_NEW(struct mg_mgr);
    mg_mgr_init(Manager, NULL);

    MR_GC_register_finalizer(Manager, MMG_mgr_finalize, NULL);
").

:- pragma foreign_proc("C",
    bind(Manager::in, Address::in, Handler::in(event_handler_pred),
         Connection::uo, _IO0::di, _IO::uo),
    [promise_pure, may_call_mercury],
"
    struct mg_bind_opts opts = {
        (void*)Handler,
        0,
        NULL
    };
    Connection = mg_bind_opt(Manager, Address,
        (mg_event_handler_t)MMG_event_handler, opts);
").

:- pragma foreign_code("C",
"
void
MMG_mgr_finalize(void *mg_mgr, void *user_data)
{
    mg_mgr_free((struct mg_mgr *)mgr);
}
").

:- pragma foreign_proc("C",
    poll(Server::in, Loop::in, Milliseconds::in, _IO0::di, _IO::uo),
    [promise_pure, may_call_mercury],
"
    do {
        mg_mgr_poll(Server, Milliseconds);
    } while (Loop);
").

%----------------------------------------------------------------------------%
%
% Enable the server to call the HTTP handler written in Mercury.
% XXX: Not sure if this is legal Mercury.
%

:- pred event_handler_wrapper : event_handler_pred
    `with_inst` event_handler_pred.

:- pragma foreign_export("C", event_handler_wrapper(in, in, di, uo),
    "MMG_event_handler").

event_handler_wrapper(Connection, Event, !IO) :-
    (Connection ^ event_handler)(Connection, Event, !IO).

%----------------------------------------------------------------------------%
%
% Implementation of connection properties and I/O.
%

%:- pragma foreign_proc("C",
%    event_handler(Connection::in) = (Handler::out(event_handler_pred)),
%    [promise_pure, will_not_call_mercury],
%"
%    Handler = (MR_Word)(Connection->user_data);
%").

%:- pragma foreign_proc("C",
%    request_path(Connection::in) = (Uri::out),
%    [promise_pure, will_not_call_mercury],
%"
%    Uri = (MR_String)(Connection->uri);
%").

%:- pragma foreign_proc("C", http_version(Connection::in) = (Version::out),
%    [promise_pure, will_not_call_mercury],
%"
%    Version = (MR_String)(Connection->http_version);
%").

%:- pragma foreign_proc("C", is_websocket(Connection::in),
%    [promise_pure, will_not_call_mercury],
%"
%    SUCCESS_INDICATOR = Connection->is_websocket ? MR_TRUE : MR_FALSE;
%").

%----------------------------------------------------------------------------%

%Connection ^ remote_address =
%    socket_address(Connection ^ remote_ip, Connection ^ remote_port).
%
%:- pragma foreign_proc("C", remote_ip(Connection::in) = (IP::out),
%    [promise_pure, will_not_call_mercury],
%"
%    /* NOTE: Const cast */
%    IP = (MR_String)((MR_ConstString)(Connection->remote_ip));
%").

%:- pragma foreign_proc("C", remote_port(Connection::in) = (Port::out),
%    [promise_pure, will_not_call_mercury],
%"
%    Port = (MR_Integer)(Connection->remote_port);
%").

%----------------------------------------------------------------------------%

%Connection ^ local_address =
%    socket_address(Connection ^ local_ip, Connection ^ local_port).

%:- pragma foreign_proc("C", local_ip(Connection::in) = (IP::out),
%    [promise_pure, will_not_call_mercury],
%"
%    /* NOTE: Const cast */
%    IP = (MR_String)((MR_ConstString)(Connection->local_ip));
%").

%:- pragma foreign_proc("C", local_port(Connection::in) = (Port::out),
%    [promise_pure, will_not_call_mercury],
%"
%    Port = (MR_Integer)(Connection->local_port);
%").

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

%:- pragma foreign_proc("C",
%    get_status(Connection::in, Status::out, _IO0::di, _IO::uo),
%    [promise_pure, will_not_call_mercury],
%"
%    Status = Connection->status_code;
%").

%:- pragma foreign_proc("C",
%    send_status(Connection::in, Status::in, _IO0::di, _IO::uo),
%    [promise_pure, will_not_call_mercury],
%"
%    mg_send_status(Connection, Status);
%").

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
