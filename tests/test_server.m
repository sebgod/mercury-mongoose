%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: test_server.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Fri 19 Dec 18:31:53 CST 2014
% Stability: low
%----------------------------------------------------------------------------%
% Testing the mongoose API defined in src/
%----------------------------------------------------------------------------%

:- module test_server.

:- interface.

:- import_module io.

%----------------------------------------------------------------------------%

:- pred main(io::di, io::uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module mercury_mongoose.

:- import_module bool.
:- import_module list.
:- import_module string.

%----------------------------------------------------------------------------%

:- func echo_server `with_type` handler_func `with_inst` handler_func.

echo_server(Connection, Event, !IO) = Result :-
    ( Event = auth ->
        Result = true
    ; Event = request ->
        send_header(Connection, "Content-Type",
            "text/plain; charset=utf-8", !IO),
        get_status(Connection, Status, !IO),
        printf_data(Connection,
            "Connection properties:
            Hello! Requested URI is [%s]
            Remote IP Address: %s:%d
            Local IP Address: %s:%d
            HTTP Status: %d
            Is Websocket: %s",
            [s(Connection ^ requested_uri),
             s(Connection ^ remote_ip),
             i(Connection ^ remote_port),
             s(Connection ^ local_ip),
             i(Connection ^ local_port),
             i(Status),
             s(( if is_websocket(Connection) then "yes" else "no" ))
            ], _, !IO),
        Result = true
    ;
        Result = false
    ).

main(!IO) :-
    create(Server, echo_server, !IO),
    set_option(Server, listening_port, port(8080), !IO),
    poll(Server, yes, 1000, !IO),
    destroy(Server, !IO).

%----------------------------------------------------------------------------%
:- end_module test_server.
%----------------------------------------------------------------------------%
