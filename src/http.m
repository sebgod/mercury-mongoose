%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: http.m
% Copyright © 2015 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet@outlook.com>
% Created on: Fri Oct  2 23:10:20 CST 2015
% Stability: low
%----------------------------------------------------------------------------%
% HTTP and websocket functionality.
%----------------------------------------------------------------------------%

:- module mercury_mongoose.http.

:- interface.

:- type websocket_opcode
    --->    continuation
    ;       text
    ;       binary
    ;       connection_close
    ;       ping
    ;       pong.

:- type http_status == int.

:- type http_version == string.

%----------------------------------------------------------------------------%

:- func http_msg ^ http_msg_to_string = string.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

%----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    http_msg_to_string(HttpMsg::in) = (String::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    String = MMG_buf_to_string(HttpMsg->message);
").


%----------------------------------------------------------------------------%
:- end_module mercury_mongoose.http.
%----------------------------------------------------------------------------%
