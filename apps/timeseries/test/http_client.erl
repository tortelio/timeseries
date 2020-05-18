-module(http_client).

-compile({no_auto_import, [get/2]}).

-export([connect/2,
         disconnect/1,
         get/2,
         post/3]).

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
    Headers = [{<<"accept">>, <<"application/json">>}],
    StreamRef = gun:get(Conn, Path, Headers),
    {response, nofin, 200, _Headers} = gun:await(Conn, StreamRef, 50000),
    {ok, Body} = gun:await_body(Conn, StreamRef, 50000),
    jiffy:decode(Body, [return_maps]).

post(Conn, Path, Data) ->
    Headers = [{<<"content-type">>, <<"application/json">>}],
    StreamRef = gun:post(Conn, Path, Headers, jiffy:encode(Data)),
    {response, nofin, 200, _Headers} = gun:await(Conn, StreamRef, 50000),
    {ok, Body} = gun:await_body(Conn, StreamRef, 50000),
    jiffy:decode(Body, [return_maps]).
