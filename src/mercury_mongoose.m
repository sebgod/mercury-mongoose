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

%----------------------------------------------------------------------------%

:- type server.

:- type server_param
    --->    none.

:- type connection.

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

    % create(ServerParam, Handler, Server, !IO)
:- pred create(server_param::in, /*handler_func::in(handler_func),*/
    server::uo, io::di, io::uo) is det.

    % destroy(Server, !IO)
:- pred destroy(server::in, io::di, io::uo) is det.

    % poll(Server, Loop, Milliseconds, !IO)
:- pred poll(server::in, bool::in, int::in, io::di, io::uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

%----------------------------------------------------------------------------%

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
    create(_ServerParam::in, /* Handler::in(handler_func),*/ Server::uo,
        _IO0::di, _IO::uo), [promise_pure],
"
    Server = mg_create_server(NULL, (mg_handler_t)test_handler);
    mg_set_option(Server, ""listening_port"", ""8080"");
").

:- pragma foreign_proc("C",
    destroy(Server::in, _IO0::di, _IO::uo), [promise_pure],
"
    mg_destroy_server(&Server);
").

:- pragma foreign_proc("C",
    poll(Server::in, Loop::in, Milliseconds::in, _IO0::di, _IO::uo),
        [promise_pure],
"
    do {
        mg_poll_server(Server, Milliseconds);
    } while (Loop);
").

%----------------------------------------------------------------------------%
%
% Test facilities
%

:- func test_handler `with_type` handler_func `with_inst` handler_func.

:- pragma foreign_export("C", test_handler(in, in, di, uo) = out,
    "test_handler").

test_handler(_Connection, Event, !IO) = Result :-
    trace [io(!Trace)] (
        io.print("received event: ", !Trace),
        io.print(Event, !Trace),
        io.nl(!Trace)
    ),
    ( Event = auth ->
        Result = true
    ;
        Result = false
    ).

%----------------------------------------------------------------------------%
:- end_module mercury_mongoose.
%----------------------------------------------------------------------------%
