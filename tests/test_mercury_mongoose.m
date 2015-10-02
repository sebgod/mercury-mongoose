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

:- pred echo_prop : event_handler_pred `with_inst` event_handler_pred.

echo_prop(_Connection, http_request(Msg), !IO) :-
        print_line("http_request: ", !IO),
        print_line(http_msg_to_string(Msg), !IO).

echo_prop(_Connection, http_reply(Msg), !IO).
echo_prop(_Connection, http_chunk(Msg), !IO).

echo_prop(_Connection, connect, !IO).
echo_prop(_Connection, poll, !IO).
echo_prop(_Connection, recv, !IO).
echo_prop(_Connection, close, !IO).

main(!IO) :-
    get_environment_var("MONGOOSE_TEST_ADDRESS", MaybeAddress, !IO),
    Address = ( if MaybeAddress = yes(Address0) then Address0 else "8080" ),
    manager_init(Manager, !IO),
    bind(Manager, Address, echo_prop, Server, !IO),
    set_protocol(Server, http_websocket, !IO),
    install_signal_handlers(!IO),
    poll(Manager, yes, 1000, !IO),
    manager_free(Manager, !IO).

%----------------------------------------------------------------------------%
:- end_module test_mercury_mongoose.
%----------------------------------------------------------------------------%
