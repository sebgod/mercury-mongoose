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
        printf_data(Connection,
            "Hello! Requested URI is [%s]
Remote IP Address: %s
Local IP Address: %s\n",
            [s(Connection ^ requested_uri),
             s(Connection ^ remote_ip),
             s(Connection ^ local_ip)
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
