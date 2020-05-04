%%%-----------------------------------------------------------------------------
%%% Copyright (C) 2020 Törteli Olivér Máté <tortelio@yahoo.com>
%%%
%%% All rights reserved.
%%%-----------------------------------------------------------------------------
%%% @doc Timeseries server
%%% @end
%%%-----------------------------------------------------------------------------

-module(timeseries_fixtures).

%%%=============================================================================
%%% Exports
%%%=============================================================================

-export([timeseries/1,
         save/1]).

%%%=============================================================================
%%% Functions
%%%=============================================================================

-spec timeseries(Id) -> Timeseries when
      Id :: dummy,
      Timeseries :: timeseries:timeseries().
timeseries(dummy) ->
  Ts1 = timeseries:new(<<"dummy">>),
  Ts2 = timeseries:add(Ts1, #{<<"t">> => 1,  <<"x">> => 2}),
  Ts3 = timeseries:add(Ts2, #{<<"t">> => 2,  <<"x">> => 3}),
  Ts4 = timeseries:add(Ts3, #{<<"t">> => 4,  <<"x">> => 6}),
  Ts5 = timeseries:add(Ts4, #{<<"t">> => 7,  <<"x">> => 12}),
  Ts6 = timeseries:add(Ts5, #{<<"t">> => 11, <<"x">> => 30}),
  Ts7 = timeseries:finish(Ts6),
  Ts7;

timeseries(dummy2) ->
  Ts1 = timeseries:new(<<"dummy2">>),
  F = fun(I, Ts) ->
              timeseries:add(Ts, #{<<"t">> => I,  <<"x">> => 2*I})
      end,
  Ts2 = lists:foldl(F, Ts1, lists:seq(1, 100)),
  Ts3 = timeseries:finish(Ts2),
  Ts3;

timeseries(dummy_long5) ->
  Ts1 = timeseries:new(<<"dummy_long">>),
  F = fun(I, Ts) ->
              timeseries:add(Ts, #{<<"t">> => I,  <<"xx">> => 2*I,  <<"yy">> => I})
      end,
  Ts2 = lists:foldl(F, Ts1, lists:seq(1, 1000)),
  Ts3 = timeseries:finish(Ts2),
  Ts3.

-spec save(Id) -> ok when
      Id :: atom().
save(Id) when is_atom(Id) ->
    timeseries_server:save(timeseries(Id)),
    ok.
