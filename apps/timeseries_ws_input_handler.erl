-module(timeseries_ws_input_handler).

-export([init/2,
         websocket_init/1,
         websocket_handle/2,
         websocket_info/2,
         terminate/3]).

-record(state, {}).

init(Req, []) ->
  io:format("Init~n~t- req: ~p~n", [Req]),
  {cowboy_websocket, Req, #state{}, #{idle_timeout => 3600000}}.

websocket_init(#state{} = State) ->
  io:format("Websocket init~n~t- state: ~p~n", [State]),
  {ok, State}.

websocket_handle(Msg, State) ->
  io:format("Websocket handle~n~t- message: ~p~n~t- state: ~p~n", [Msg, State]),
  {ok, State}.

websocket_info(Info, State) ->
  io:format("Websocket info~n~t- info: ~p~n~t- state: ~p~n", [Info, State]),
  {ok, State}.

terminate(Reason, PartialReq, State) ->
  io:format("Websocket info~n~t- reason: ~p~n~t- partion request ~p~n~t- state: ~p~n",
            [Reason, PartialReq, State]),
  ok.
