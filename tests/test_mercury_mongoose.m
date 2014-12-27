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
:- import_module mercury_mongoose.path_util.

:- import_module bool.
:- import_module list.
:- import_module string.

%----------------------------------------------------------------------------%

:- func echo_prop `with_type` handler_func `with_inst` handler_func.

    % TODO: Use maybe_error/1 for return value
echo_prop(Connection, Event, !IO) = HandlerResult :-
    ( if Event = auth then
        HandlerResult = true
    else if Event = request then
        RequestPath = Connection ^ request_path,
        PathElements = split_path(RequestPath),
        ( if
            PathElements = ["internal", File | _]
        then
            ( if File = "remote_ip" then
                Prop = Connection ^ remote_ip
            else if File = "local_ip" then
                Prop = Connection ^ local_ip
            else if File = "is_websocket" then
                Prop = ( if is_websocket(Connection) then "yes" else "no" )
            else
                send_status(Connection, 404, !IO),
                Prop = "<property not supported!>"
            )
        else
            send_status(Connection, 403, !IO),
            Prop = "<path not allowed>"
        ),
        send_header(Connection, "Content-Type", "application/json", !IO),
        get_status(Connection, StatusCode, !IO),
        printf_data(Connection, "{ \"%s\": \"%s\", \"Status\": %d }",
            [s(to_encoded_string(RequestPath)), s(Prop), i(StatusCode)],
            _Bytes,
            !IO),
        HandlerResult = true
    else
        HandlerResult = false
    ).

main(!IO) :-
    create(Server, echo_prop, !IO),
    set_option(Server, listening_port, port(8080), !IO),
    poll(Server, yes, 1000, !IO),
    destroy(Server, !IO).

%----------------------------------------------------------------------------%
:- end_module test_mercury_mongoose.
%----------------------------------------------------------------------------%
