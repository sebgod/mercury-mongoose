%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: test_mercury_mongoose.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Fri 19 Dec 18:31:53 CST 2014
% Stability: low
%----------------------------------------------------------------------------%
% Testing the mongoose API defined in src/
%----------------------------------------------------------------------------%

:- module test_mercury_mongoose.

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

:- func echo_prop `with_type` handler_func `with_inst` handler_func.

echo_prop(Connection, Event, !IO) = Result :-
    ( Event = auth ->
        Result = true
    ; Event = request ->
        Uri = Connection ^ requested_uri,
        ( if Uri = "/remote_ip" then
            Prop = Connection ^ remote_ip
        else if Uri = "/local_ip" then
            Prop = Connection ^ local_ip
        else if Uri = "/is_websocket" then
            Prop = ( if is_websocket(Connection) then "yes" else "no" )
        else
            send_status(Connection, 404, !IO),
            Prop = "<file not found>"
        ),
        send_header(Connection, "Content-Type", "application/json", !IO),
        get_status(Connection, StatusCode, !IO),
        printf_data(Connection, "{ \"%s\": \"%s\", \"Status\": %d }",
            [s(Uri), s(Prop), i(StatusCode)], _Bytes, !IO),
        Result = true
    ;
        Result = false
    ).

main(!IO) :-
    create(Server, echo_prop, !IO),
    set_option(Server, listening_port, port(8080), !IO),
    poll(Server, yes, 1000, !IO),
    destroy(Server, !IO).

%----------------------------------------------------------------------------%
:- end_module test_mercury_mongoose.
%----------------------------------------------------------------------------%
