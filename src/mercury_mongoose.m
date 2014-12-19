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
%----------------------------------------------------------------------------%

:- module mercury_mongoose.

:- interface.

:- import_module io.

%----------------------------------------------------------------------------%

:- type server.

:- type connection.

:- type callback.

:- type callback_result
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

:- pred create(c_pointer::in, callback::in, server::uo, io::di, io::uo)
    is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

%----------------------------------------------------------------------------%

:- pragma foreign_decl("C", include_file("mongoose.h")).

:- pragma foreign_type("C", server,
    "struct mg_server *", [can_pass_as_mercury_type]).

:- pragma foreign_type("C", connection,
    "struct mg_connection *", [can_pass_as_mercury_type]).

:- pragma foreign_type("C", callback, "mg_handler_t").

:- pragma foreign_enum("C", callback_result/0,
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
    create(ServerParam::in, Handler::in, Server::uo, _IO0::di, _IO::uo),
    [promise_pure],
"
    Server = mg_create_server((void *)ServerParam, Handler);
").

%----------------------------------------------------------------------------%
:- end_module mercury_mongoose.
%----------------------------------------------------------------------------%
