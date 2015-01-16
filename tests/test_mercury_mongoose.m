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
:- import_module int.
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
        RequestPath   = Connection ^ request_path,
        ( if
            match_prefix_and_suffix(RequestPath, "/internal/", File)
        then
            ( if File = ""; File = "index.html" then
                send_file(Connection, "internal/index.html", !IO),
                Data = {no, more}
            else if File = "remote_ip" then
                Data = {yes(Connection ^ remote_ip), true}
            else if File = "local_ip" then
                Data = {yes(Connection ^ local_ip), true}
            else if File = "is_websocket" then
                Data = {yes(is_websocket(Connection) -> "yes" ; "no"), true}
            else
                send_status(Connection, 404, !IO),
                Data = {no, false}
            )
        else
            Data = {no, false}
        ),
        ( if Data = {yes(Value), _} then
            send_header(Connection, "Content-Type",
                "application/json; charset=utf-8", !IO),
            get_status(Connection, StatusCode, !IO),
            printf_data(Connection,
                "{ \"%s\": \"%s\", \"Status\": %d }",
                [s(RequestPath), s(Value), i(StatusCode)],
                _Bytes,
                !IO)
        else
            true
        ),
        Data = {_, HandlerResult}
    else
        HandlerResult = false
    ).

main(!IO) :-
    get_environment_var("MONGOOSE_TEST_PORT", MaybePort, !IO),
    Port = ( if
        MaybePort = yes(PortStr),
        to_int(PortStr, PortNumber)
    then
        port(PortNumber)
    else
        port(8080)
    ),
    create(Server, echo_prop, !IO),
    set_option(Server, document_root,  path("."), !IO),
    set_option(Server, listening_port, Port, !IO),
    poll(Server, yes, 1000, !IO),
    destroy(Server, !IO).

%----------------------------------------------------------------------------%
:- end_module test_mercury_mongoose.
%----------------------------------------------------------------------------%
