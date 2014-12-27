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
:- import_module maybe.
:- import_module string.

%----------------------------------------------------------------------------%

:- func echo_prop `with_type` handler_func `with_inst` handler_func.

    % TODO: Use maybe_error/1 for return value
echo_prop(Connection, Event, !IO) = HandlerResult :-
    ( if Event = auth then
        HandlerResult = true
    else if Event = request then
        RequestPath = Connection ^ request_path,
        PathElements = split_and_decode_path(RequestPath),
        ( if
            PathElements = ["", "internal", File | _]
        then
            ( if File = ""; File = "index.html" then
                send_file(Connection, "internal/index.html", !IO),
                MaybeData = no
            else if File = "remote_ip" then
                MaybeData = yes(Connection ^ remote_ip)
            else if File = "local_ip" then
                MaybeData = yes(Connection ^ local_ip)
            else if File = "is_websocket" then
                MaybeData = yes(is_websocket(Connection) -> "yes" ; "no")
            else
                send_status(Connection, 404, !IO),
                MaybeData = no
            ),
            HandlerResult = (MaybeData = no -> more ; true)
        else
            send_status(Connection, 403, !IO),
            MaybeData = no,
            HandlerResult = true
        ),
        ( if MaybeData = yes(Data) then
            send_header(Connection, "Content-Type",
                "application/json; charset=utf-8", !IO),
            get_status(Connection, StatusCode, !IO),
            printf_data(Connection,
                "{ \"%s\": \"%s\", \"Status\": %d }",
                [s(to_encoded_string(RequestPath)), s(Data), i(StatusCode)],
                _Bytes,
                !IO)
        else
            true
        )
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
