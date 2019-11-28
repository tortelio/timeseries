-module(http_client).

-compile({no_auto_import, [get/2]}).

-export([connect/2,
         disconnect/1,
         get/2]).

connect(Url, Port) ->
  {ok, Conn} = gun:open(Url, Port),

  receive
      {gun_up, Conn, http} -> ok
  after
      5000 -> throw(missing_successful_up)
  end,

  {ok, Conn}.

disconnect(Conn) ->
    ok = gun:close(Conn).

get(Conn, Path) ->
    Headers = [{<<"content-type">>, <<"application/msgpack">>}],
    StreamRef = gun:get(Conn, Path, Headers),
    {response, nofin, 200, _Headers} = gun:await(Conn, StreamRef),
    {ok, Body} = gun:await_body(Conn, StreamRef),
    {ok, Msg} = msgpack:unpack(Body),
    Msg.
