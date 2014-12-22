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
%----------------------------------------------------------------------------%

:- module mercury_mongoose.

:- interface.

:- import_module bool.
:- import_module io.
:- import_module list.

%----------------------------------------------------------------------------%

:- type server.

:- type connection.

:- type decoded_uri == string.  % TODO: URI ADT

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
    %
:- pred poll(server::in, bool::in, int::in, io::di, io::uo) is det.

%----------------------------------------------------------------------------%
%
% Connection properties and I/O.
%

:- func (connection::in) ^ server_handler =
    (handler_func::out(handler_func)) is det.

:- func connection ^ requested_uri = decoded_uri.

:- func connection ^ remote_ip = ip_address.

:- func connection ^ local_ip = ip_address.

%----------------------------------------------------------------------------%

    % send_string_data(Connection, String, BytesWritten, !IO):
    %
:- pred send_string_data(connection::in, string::in, int::out,
    io::di, io::uo) is det.

    % printf_data(Connection, FmtString, Params, BytesWritten, !IO):
    %
:- pred printf_data(connection::in, string::in, list(poly_type)::in,
    int::out, io::di, io::uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

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
    mg_set_option(Server, ""listening_port"", ""8080"");
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

:- pragma foreign_proc("C", requested_uri(Connection::in) = (Uri::out),
    [promise_pure, will_not_call_mercury],
"
    Uri = (MR_String)(Connection->uri);
").

:- pragma foreign_proc("C", remote_ip(Connection::in) = (IP::out),
    [promise_pure, will_not_call_mercury],
"
    IP = (MR_String)(Connection->remote_ip);
").

:- pragma foreign_proc("C", local_ip(Connection::in) = (IP::out),
    [promise_pure, will_not_call_mercury],
"
    IP = (MR_String)(Connection->local_ip);
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

%----------------------------------------------------------------------------%
:- end_module mercury_mongoose.
%----------------------------------------------------------------------------%
