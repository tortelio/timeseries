-module(ws_client).

-export([connect/3,
         disconnect/1,
         send/2,
         send_for_reply/2,
         take/1]).

connect(Url, Port, WSPath) ->
    {ok, Conn} = http_client:connect(Url, Port),

    _Ref = gun:ws_upgrade(Conn, WSPath),

    ok =
    receive
        {gun_upgrade, Conn, _Ref, [<<"websocket">>], _Headers} ->
            ok
    after
        5000 -> throw(missing_successful_upgrading)
    end,

    {ok, Conn}.

% TODO decide is this ok
% maybe `gun:close/3' should be called
disconnect(Conn) ->
    ok = gun:ws_send(Conn, close),
    ok.

send(Conn, Msg) ->
    ok = gun:ws_send(Conn, {binary, msgpack:pack(Msg)}),
    ok.

send_for_reply(Conn, Msg) ->
    ok = send(Conn, Msg),
    Repsonse = take(Conn),
    Repsonse.

take(Conn) ->
    receive
        {gun_ws, Conn, _Ref, {binary, Data}} ->
            {ok, Msg} = msgpack:unpack(Data),
            Msg
    after
        5000 ->
            throw({missing_message, Conn})
    end.
