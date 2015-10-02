%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: mercury_mongoose.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet@outlook.com>
% Created on: Thu 18 Dec 17:38:25 CST 2014
% Stability: low
%----------------------------------------------------------------------------%
% API to access the lightwight mongoose HTTP(S) web-server.
% The open-source (GPLv2) version is used for this library, i.e. that
% means that this library is also licensed under the GPLv2.
% c.f.: https://github.com/cesanta/mongoose
%       http://en.wikipedia.org/wiki/Mongoose_(web_server)
%
% TODO: check if http://www.microhowto.info/howto/
%                serve_web_pages_using_an_embedded_http_server_in_java.html
%       is compatible with the mongoose api.
%----------------------------------------------------------------------------%

:- module mercury_mongoose.

:- interface.

:- include_module mercury_mongoose.http.
:- include_module mercury_mongoose.path_util.
:- include_module mercury_mongoose.signal.

:- import_module bool.
:- import_module io.
:- import_module list.

%----------------------------------------------------------------------------%

:- type manager.

:- type connection.

:- type event_handler_pred == pred(connection, event_data, io, io).
:- inst event_handler_pred == (pred(in, in(event_data), di, uo) is det).

:- type (event)
    --->    poll
    ;       connect
    ;       http_request
    ;       http_reply
    ;       http_chunk
    ;       recv
    ;       close
    .

:- type http_msg.

:- type event_data
    --->    poll
    ;       connect
    ;       http_request(http_msg)
    ;       http_reply(http_msg)
    ;       http_chunk(http_msg)
    ;       recv
    ;       close
    .

:- inst event_data ==
    unique( close
          ; connect
          ; http_chunk(ground)
          ; http_reply(ground)
          ; http_request(ground)
          ; poll
          ; recv
          ).

:- type protocol
    --->    http_websocket
    ;       dns
    .

%----------------------------------------------------------------------------%
%
% Mongoose public interface.
%

    % manager_init(Manager, !IO):
    %
:- pred manager_init(manager::uo, io::di, io::uo) is det.

    % manager_free(Manager, !IO):
    %
:- pred manager_free(manager::in, io::di, io::uo) is det.

    % bind(Manager, Address, Handler, Connection, !IO):
    %
:- pred bind(manager::in, string::in,
        event_handler_pred::in(event_handler_pred),
        connection::uo, io::di, io::uo) is det.

    % poll(Server, Loop, Milliseconds, !IO):
    %
    % TODO: Create loop type to stop on signals, etc.
    %
:- pred poll(manager::in, bool::in, int::in, io::di, io::uo) is det.

    % set_protocol(Connection, Protocol):
    %
:- pred set_protocol(connection::in, protocol::in, io::di, io::uo)
    is det.

%----------------------------------------------------------------------------%
%
% Connection properties and I/O.
%

:- func (connection::in) ^ event_handler =
    (event_handler_pred::out(event_handler_pred)) is det.

%----------------------------------------------------------------------------%

    % send_string(Connection, String, BytesWritten, !IO):
    %
:- pred send_string(connection::in, string::in, int::out,
    io::di, io::uo) is det.

    % send_format(Connection, FmtString, Params, BytesWritten, !IO):
    %
:- pred send_format(connection::in, string::in, list(poly_type)::in,
    int::out, io::di, io::uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module int. % for int_to_string/1, det_to_int/1
:- import_module string.
:- import_module unit.

%----------------------------------------------------------------------------%
%
% Implemtation of the Mongoose public API in C
%

    % `include_file()' is preferred over `#include', since then linking
    % to the library does not require the presence of the API header
    %
:- pragma foreign_decl("C", include_file("mongoose.h")).


:- pragma foreign_type("C", manager,
    "struct mg_mgr *", [can_pass_as_mercury_type]).

:- pragma foreign_type("C", connection,
    "struct mg_connection *", [can_pass_as_mercury_type]).

:- pragma foreign_type("C", http_msg,
    "struct http_message *", [can_pass_as_mercury_type]).

:- pragma foreign_enum("C", (event)/0,
    [
        poll         - "MG_EV_POLL",
        connect      - "MG_EV_CONNECT",
        http_request - "MG_EV_HTTP_REQUEST",
        http_reply   - "MG_EV_HTTP_REPLY",
        http_chunk   - "MG_EV_HTTP_CHUNK",
        recv         - "MG_EV_RECV",
        close        - "MG_EV_CLOSE"
    ]).

:- pragma foreign_proc("C",
    manager_init(Manager::uo, _IO0::di, _IO::uo),
    [promise_pure, may_call_mercury],
"
    Manager = MR_GC_NEW(struct mg_mgr);
    mg_mgr_init(Manager, NULL);
").

:- mutable(signal_val, int, 0, ground,
    [untrailed, attach_to_io_state, foreign_name("C", "MMG_signal_val")]).

:- pragma foreign_proc("C",
    bind(Manager::in, Address::in, Handler::in(event_handler_pred),
         Connection::uo, _IO0::di, _IO::uo),
    [promise_pure, may_call_mercury],
"
    struct mg_bind_opts opts = {
        (void*)Handler,
        0,
        NULL
    };
    Connection = mg_bind_opt(Manager, Address,
        (mg_event_handler_t)MMG_event_handler, opts);
").

:- pragma foreign_proc("C",
    manager_free(Manager::in, _IO0::di, _IO::uo),
    [promise_pure, may_call_mercury],
"
    mg_mgr_free(Manager);
").

:- pragma foreign_proc("C",
    poll(Server::in, Loop::in, Milliseconds::in, _IO0::di, _IO::uo),
    [promise_pure, may_call_mercury],
"
    do {
        mg_mgr_poll(Server, Milliseconds);
    } while (Loop && !MMG_signal_val);
").

:- pragma foreign_export_enum("C", protocol/0,
    [prefix("MG_PROTO_"), uppercase]).

:- pragma foreign_proc("C",
    set_protocol(Connection::in, Protocol::in, _IO0::di, _IO::uo),
    [promise_pure, may_call_mercury],
"
    switch (Protocol) {
        case MG_PROTO_HTTP_WEBSOCKET:
            mg_set_protocol_http_websocket(Connection);
            break;

        case MG_PROTO_DNS:
            mg_set_protocol_dns(Connection);
            break;
    }
").

:- pragma foreign_export("C", left(in, in) = (out), "MMG_left").

:- pragma foreign_decl("C",
"
#define MMG_buf_to_string(buf) MMG_left((MR_String)((buf).p), (buf).len)
").

%----------------------------------------------------------------------------%
%
% Enable the server to call the event handler written in Mercury.
% XXX: Not sure if this is legal Mercury.
%

:- pred event_handler_wrapper(connection::in, (event)::in, c_pointer::in,
    io::di, io::uo) is det.

:- pragma foreign_export("C", event_handler_wrapper(in, in, in, di, uo),
    "MMG_event_handler").

event_handler_wrapper(Connection, Event, DataPtr, !IO) :-
    ( Event = http_request,
        Data = http_request(data_ptr_to_any(DataPtr))
    ; Event = http_reply,
        Data = http_reply(data_ptr_to_any(DataPtr))
    ; Event = http_chunk,
        Data = http_chunk(data_ptr_to_any(DataPtr))
    ; Event = connect,
        Data = connect
    ; Event = poll,
        Data = poll
    ; Event = close,
        Data = close
    ; Event = recv,
        Data = recv
    ),
    (Connection ^ event_handler)(Connection, Data, !IO).

:- func data_ptr_to_any(c_pointer) = T.

:- pragma foreign_proc("C", data_ptr_to_any(DataPtr::in) = (Value::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    Value = DataPtr;
").

%----------------------------------------------------------------------------%
%
% Implementation of connection properties and I/O.
%

:- pragma foreign_proc("C",
    event_handler(Connection::in) = (Handler::out(event_handler_pred)),
    [promise_pure, will_not_call_mercury],
"
    Handler = (MR_Word)(Connection->user_data);
").

%----------------------------------------------------------------------------%

:- pragma foreign_decl("C", local, "#include <string.h>").

:- pragma foreign_proc("C",
    send_string(Connection::in, String::in, BytesWritten::out,
        _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    BytesWritten = mg_send(Connection, String, strlen(String));
").

send_format(Connection, FmtString, Params, BytesWritten, !IO) :-
    send_string(Connection,
        string.format(FmtString, Params),
        BytesWritten, !IO).

%----------------------------------------------------------------------------%
:- end_module mercury_mongoose.
%----------------------------------------------------------------------------%
