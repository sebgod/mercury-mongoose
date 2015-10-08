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
%       https://docs.cesanta.com/mongoose/
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

:- type event_handler_pred      == pred(connection, event_data, io, io).
:- inst event_handler_pred(I)   == (pred(in, in(I), di, uo) is det).
:- inst event_handler_pred      == event_handler_pred(event_data).
:- inst http_event_handler_pred == event_handler_pred(http_event_data).

:- type (event)
    % COAP events
    --->    coap_ack
    ;       coap_con
    ;       coap_noc
    ;       coap_rst
    % MQTT events
    ;       mqtt_connack
    ;       mqtt_connack_accepted
    ;       mqtt_connack_bad_auth
    ;       mqtt_connack_identifier_rejected
    ;       mqtt_connack_not_authorized
    ;       mqtt_connack_server_unavailable
    ;       mqtt_connack_unacceptable_version
    ;       mqtt_connect
    ;       mqtt_disconnect
    ;       mqtt_pingeq
    ;       mqtt_pingresp
    ;       mqtt_puback
    ;       mqtt_pubcomp
    ;       mqtt_publish
    ;       mqtt_pubrec
    ;       mqtt_pubrel
    ;       mqtt_suback
    ;       mqtt_subscribe
    ;       mqtt_unsuback
    ;       mqtt_unsubscribe
    % general connection events
    ;       accept
    ;       connect
    ;       close
    ;       poll
    ;       recv
    ;       send
    % HTTP and websocket events
    ;       http_chunk
    ;       http_reply
    ;       http_request
    ;       ssi_call
    ;       websocket_control_frame
    ;       websocket_frame
    ;       websocket_handshake_done
    ;       websocket_handshake_request
    .

:- type http_msg.

:- type error_code.

:- type event_data
    % COAP events
    --->    coap_ack
    ;       coap_con
    ;       coap_noc
    ;       coap_rst
    % MQTT events
    ;       mqtt_connack
    ;       mqtt_connack_accepted
    ;       mqtt_connack_bad_auth
    ;       mqtt_connack_identifier_rejected
    ;       mqtt_connack_not_authorized
    ;       mqtt_connack_server_unavailable
    ;       mqtt_connack_unacceptable_version
    ;       mqtt_connect
    ;       mqtt_disconnect
    ;       mqtt_pingeq
    ;       mqtt_pingresp
    ;       mqtt_puback
    ;       mqtt_pubcomp
    ;       mqtt_publish
    ;       mqtt_pubrec
    ;       mqtt_pubrel
    ;       mqtt_suback
    ;       mqtt_subscribe
    ;       mqtt_unsuback
    ;       mqtt_unsubscribe
    % general connection events
    ;       connect(error_code)
    ;       close
    ;       poll
    ;       recv
    ;       send
    % HTTP and websocket events
    ;       http_chunk(http_msg)
    ;       http_reply(http_msg)
    ;       http_request(http_msg)
    ;       ssi_call(string)
    ;       websocket_control_frame
    ;       websocket_frame
    ;       websocket_handshake_done
    ;       websocket_handshake_request
    .

:- inst event_data
    % COAP events
    --->    coap_ack
    ;       coap_con
    ;       coap_noc
    ;       coap_rst
    % MQTT events
    ;       mqtt_connack
    ;       mqtt_connack_accepted
    ;       mqtt_connack_bad_auth
    ;       mqtt_connack_identifier_rejected
    ;       mqtt_connack_not_authorized
    ;       mqtt_connack_server_unavailable
    ;       mqtt_connack_unacceptable_version
    ;       mqtt_connect
    ;       mqtt_disconnect
    ;       mqtt_pingeq
    ;       mqtt_pingresp
    ;       mqtt_puback
    ;       mqtt_pubcomp
    ;       mqtt_publish
    ;       mqtt_pubrec
    ;       mqtt_pubrel
    ;       mqtt_suback
    ;       mqtt_subscribe
    ;       mqtt_unsuback
    ;       mqtt_unsubscribe
    % general connection events
    ;       connect(ground)
    ;       close
    ;       poll
    ;       recv
    ;       send
    % HTTP and websocket events
    ;       http_chunk(ground)
    ;       http_reply(ground)
    ;       http_request(ground)
    ;       ssi_call(ground)
    ;       websocket_control_frame
    ;       websocket_frame
    ;       websocket_handshake_done
    ;       websocket_handshake_request
    .

:- inst http_event_data ==
    unique(
    % general connection events
            connect(ground)
    ;       close
    ;       poll
    ;       recv
    ;       send
    % HTTP and websocket events
    ;       http_chunk(ground)
    ;       http_reply(ground)
    ;       http_request(ground)
    ;       ssi_call(ground)
    ;       websocket_control_frame
    ;       websocket_frame
    ;       websocket_handshake_done
    ;       websocket_handshake_request
    ).

:- type protocol
    --->    none
    ;       http_websocket
    ;       dns
    ;       mqtt
    .

:- inst http_websocket ---> http_websocket.

%----------------------------------------------------------------------------%
%
% Mongoose manager API.
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
        event_handler_pred::in(event_handler_pred(I)),
        connection::uo, io::di, io::uo) is det.

    % poll(Manager, Loop, Milliseconds, !IO):
    %
:- pred poll(manager::in, bool::in, int::in, io::di, io::uo) is det.

%----------------------------------------------------------------------------%
%
% Connection properties and I/O.
%

    % set_protocol(Connection, Protocol):
    %
:- pred set_protocol(connection::in, protocol::in, io::di, io::uo)
    is det.

:- func connection ^ connection_protocol = protocol.

    % send_string(Connection, String, BytesWritten, !IO):
    %
:- pred send_string(connection::in, string::in, int::out,
    io::di, io::uo) is det.

    % send_format(Connection, FmtString, Params, BytesWritten, !IO):
    %
:- pred send_format(connection::in, string::in, list(poly_type)::in,
    int::out, io::di, io::uo) is det.

    % is_success(ErrorCode):
    % succeeds if error_code indicates success.
    %
:- pred is_success(error_code::in) is semidet.

:- pred enable_multithreading(connection::in, io::di, io::uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module int. % for int_to_string/1, det_to_int/1
:- import_module string.

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

:- pragma foreign_type("C", error_code, "int *", [can_pass_as_mercury_type]).

:- pragma foreign_enum("C", (event)/0,
    [
        coap_ack        - "MG_EV_COAP_ACK",
        coap_con        - "MG_EV_COAP_CON",
        coap_noc        - "MG_EV_COAP_NOC",
        coap_rst        - "MG_EV_COAP_RST",

        mqtt_connack    - "MG_EV_MQTT_CONNACK",
        mqtt_connack_accepted - "MG_EV_MQTT_CONNACK_ACCEPTED",
        mqtt_connack_bad_auth - "MG_EV_MQTT_CONNACK_BAD_AUTH",
        mqtt_connack_identifier_rejected -
            "MG_EV_MQTT_CONNACK_IDENTIFIER_REJECTED",
        mqtt_connack_not_authorized -
            "MG_EV_MQTT_CONNACK_NOT_AUTHORIZED",
        mqtt_connack_server_unavailable -
            "MG_EV_MQTT_CONNACK_SERVER_UNAVAILABLE",
        mqtt_connack_unacceptable_version -
            "MG_EV_MQTT_CONNACK_UNACCEPTABLE_VERSION",
        mqtt_connect     - "MG_EV_MQTT_CONNECT",
        mqtt_disconnect  - "MG_EV_MQTT_DISCONNECT",
        mqtt_pingeq      - "MG_EV_MQTT_PINGREQ",
        mqtt_pingresp    - "MG_EV_MQTT_PINGRESP",
        mqtt_puback      - "MG_EV_MQTT_PUBACK",
        mqtt_pubcomp     - "MG_EV_MQTT_PUBCOMP",
        mqtt_publish     - "MG_EV_MQTT_PUBLISH",
        mqtt_pubrec      - "MG_EV_MQTT_PUBREC",
        mqtt_pubrel      - "MG_EV_MQTT_PUBREL",
        mqtt_suback      - "MG_EV_MQTT_SUBACK",
        mqtt_subscribe   - "MG_EV_MQTT_SUBSCRIBE",
        mqtt_unsuback    - "MG_EV_MQTT_UNSUBACK",
        mqtt_unsubscribe - "MG_EV_MQTT_UNSUBSCRIBE",

        accept          - "MG_EV_ACCEPT",
        connect         - "MG_EV_CONNECT",
        close           - "MG_EV_CLOSE",
        poll            - "MG_EV_POLL",
        recv            - "MG_EV_RECV",
        send            - "MG_EV_SEND",

        http_chunk      - "MG_EV_HTTP_CHUNK",
        http_reply      - "MG_EV_HTTP_REPLY",
        http_request    - "MG_EV_HTTP_REQUEST",
        ssi_call        - "MG_EV_SSI_CALL",
        websocket_control_frame     - "MG_EV_WEBSOCKET_CONTROL_FRAME",
        websocket_frame             - "MG_EV_WEBSOCKET_FRAME",
        websocket_handshake_done    - "MG_EV_WEBSOCKET_HANDSHAKE_DONE",
        websocket_handshake_request - "MG_EV_WEBSOCKET_HANDSHAKE_REQUEST"
    ]).

:- pragma foreign_proc("C",
    manager_init(Manager::uo, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    Manager = MR_GC_NEW(struct mg_mgr);
    mg_mgr_init(Manager, NULL);
").

:- mutable(signal_val, int, 0, ground,
    [untrailed, attach_to_io_state, foreign_name("C", "MMG_signal_val")]).

:- pragma foreign_proc("C",
    bind(Manager::in, Address::in, Handler::in(event_handler_pred(I)),
         Connection::uo, _IO0::di, _IO::uo),
    [promise_pure, may_call_mercury, thread_safe],
"
    struct MMG_connection_handler_data * handler_data =
        MR_GC_NEW(struct MMG_connection_handler_data);
    struct mg_bind_opts opts = {
        handler_data,
        0,
        NULL
    };

    handler_data->proto = MG_PROTO_NONE;
    handler_data->mercury_callback = Handler;
    handler_data->mt_channel = -1;

    Connection = mg_bind_opt(Manager, Address,
        (mg_event_handler_t)MMG_event_handler, opts);
").

:- pragma foreign_proc("C",
    manager_free(Manager::in, _IO0::di, _IO::uo),
    [promise_pure, may_call_mercury, thread_safe],
"
    mg_mgr_free(Manager);
").

:- pragma foreign_proc("C",
    poll(Server::in, Loop::in, Milliseconds::in, _IO0::di, _IO::uo),
    [promise_pure, may_call_mercury, thread_safe],
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

    MMG_user_to_handler_data(Connection)->proto =
        Connection->proto_handler ? Protocol : MG_PROTO_NONE;
").

:- pragma foreign_export("C", left(in, in) = (out), "MMG_left").

:- pragma foreign_decl("C",
"
#define MMG_buf_to_string(buf) MMG_left((MR_String)((buf).p), (buf).len)
").

%----------------------------------------------------------------------------%
%
% Enable the server to call the event handler written in Mercury.
%

:- pragma foreign_proc("C", is_success(ErrorCode::in),
    [promise_pure, thread_safe, will_not_call_mercury],
"
    SUCCESS_INDICATOR = !*(ErrorCode);
").

:- pred event_handler_wrapper(connection::in, (event)::in, c_pointer::in,
    io::di, io::uo) is det.

:- pragma foreign_export("C", event_handler_wrapper(in, in, in, di, uo),
    "MMG_event_handler").

event_handler_wrapper(Connection, Event, DataPtr, !IO) :-
    unpack_handler_data(Connection, EventHandler, Protocol, ChannelIndex),
    ( if
        should_handle_event(Event, Protocol),
        ( Event = http_request,
            Data = http_request(data_ptr_to_any(DataPtr))
        ; Event = http_reply,
            Data = http_reply(data_ptr_to_any(DataPtr))
        ; Event = http_chunk,
            Data = http_chunk(data_ptr_to_any(DataPtr))
        ; Event = ssi_call,
            Data = ssi_call(data_ptr_to_any(DataPtr))
        ; Event = connect,
            Data = connect(data_ptr_to_any(DataPtr))
        ; Event = poll,
            Data = poll
        ; Event = close,
            Data = close
        ; Event = recv,
            Data = recv
        )
    then
        CurriedEventHandler = (pred(!.IO::di, !:IO::uo) is det :-
            EventHandler(Connection, Data, !IO)),
        CurriedEventHandler(!IO)
    else
        true % TODO: Log unservable event
    ).

:- func data_ptr_to_any(c_pointer) = T.

:- pragma foreign_proc("C", data_ptr_to_any(DataPtr::in) = (Value::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    Value = DataPtr;
").

:- pred should_handle_event(event, protocol).
:- mode should_handle_event(in, in) is semidet.

should_handle_event(http_request, http_websocket).
should_handle_event(http_reply,   http_websocket).
should_handle_event(http_chunk,   http_websocket).
should_handle_event(ssi_call,     http_websocket).
should_handle_event(accept,  _).
should_handle_event(connect, _).
should_handle_event(poll,    _).
should_handle_event(close,   _).
should_handle_event(recv,    _).

%----------------------------------------------------------------------------%
%
% Implementation of connection properties and I/O.
%

:- type connection_handler_data.

:- pragma foreign_decl("C",
"
struct MMG_connection_handler_data
{
    MR_Word mercury_callback;
    MR_Integer proto;
    MR_Integer mt_channel;
};

#define MMG_user_to_handler_data(Connection) \
    ((struct MMG_connection_handler_data *)(Connection->user_data))
").

:- pragma foreign_type("C", connection_handler_data,
    "struct MMG_connection_handler_data *",
    [can_pass_as_mercury_type]).

:- pred unpack_handler_data(connection::in,
    event_handler_pred::out(event_handler_pred),
    protocol::out, int::out) is det.

:- pragma foreign_proc("C",
    unpack_handler_data(Connection::in, Handler::out(event_handler_pred),
        Protocol::out, ChannelIndex::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    struct MMG_connection_handler_data * handler_data =
        MMG_user_to_handler_data(Connection);

    Handler = handler_data->mercury_callback;
    Protocol = handler_data->proto;
    ChannelIndex = handler_data->mt_channel;
").

connection_protocol(Connection) = Protocol :-
    unpack_handler_data(Connection, _, Protocol, _).

:- pragma foreign_proc("C",
    enable_multithreading(Connection::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    MMG_user_to_handler_data(Connection)->mt_channel = 0;
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
