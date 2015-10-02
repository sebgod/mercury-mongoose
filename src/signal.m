%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: signal.m
% Copyright © 2015 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet@outlook.com>
% Created on: Fri Oct  2 23:46:20 CST 2015
% Stability: low
%----------------------------------------------------------------------------%
% Signal handling for graceful manager shutdown.
%----------------------------------------------------------------------------%

:- module signal.

:- interface.

:- import_module io.

%----------------------------------------------------------------------------%

    % install_signal_handlers(!IO):
    %
    % Installs SIGINT and SIGTERM signal handlers to gracefully exit the
    % polling loop.
    %
:- pred install_signal_handlers(io::di, io::uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

%----------------------------------------------------------------------------%

:- pragma foreign_decl("C",
"
#include <mercury_signal.h>

static void
MMG_signal_handler(int signum);
").

:- pragma foreign_proc("C", install_signal_handlers(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    signal(SIGINT, MMG_signal_handler);
    signal(SIGTERM, MMG_signal_handler);
"
).

:- pragma foreign_code("C",
"
static void
MMG_signal_handler(int sig_num)
{
    signal(sig_num, MMG_signal_handler);
    MMG_signal_val = sig_num;
}
").

%----------------------------------------------------------------------------%
:- end_module signal.
%----------------------------------------------------------------------------%
