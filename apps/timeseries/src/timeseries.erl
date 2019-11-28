%%%-----------------------------------------------------------------------------
%%% Copyright (C) 2020 Törteli Olivér Máté <tortelio@yahoo.com>
%%%
%%% All rights reserved.
%%%-----------------------------------------------------------------------------
%%% @doc Timeseries object handler module
%%% @end
%%%-----------------------------------------------------------------------------

-module(timeseries).

-compile({no_auto_import, [length/1]}).

%%%=============================================================================
%%% Exports
%%%=============================================================================

% Setters
-export([new/1,
         add/2,
         finish/1]).

% Getters
-export([info/1,
         length/1,
         events/1]).

% Types
-export_type([timeseries/0,
              token/0,
              event/0,
              info/0]).

%%%=============================================================================
%%% Types
%%%=============================================================================

-record(timeseries, {token :: token(),
                     events :: [event()],
                     state :: any()}).

-type token() :: binary().
-type event() :: any(). % TODO
-type info() :: non_neg_integer().
-type timeseries() :: #timeseries{}.

%%%=============================================================================
%%% API function
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec new(Token) -> Timeseries when
      Token :: token(),
      Timeseries :: timeseries().
new(Token) when is_binary(Token) ->
    #timeseries{token = Token,
                events = [],
                state = [{created, erlang:timestamp()}]}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec add(Timeseries1, Event) -> Timeseries2 when
      Timeseries1 :: timeseries(),
      Event :: event(),
      Timeseries2 :: timeseries().
add(#timeseries{events = Events} = Timeseries, {Ts, Coord}) ->
    Timeseries#timeseries{events = Events ++ [{Ts, Coord}]}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec finish(Timeseries1) -> Timeseries2 when
      Timeseries1 :: timeseries(),
      Timeseries2 :: timeseries().
finish(#timeseries{state = [{created, _}] = State} = Timeseries) ->
    Timeseries#timeseries{state = [{finished, erlang:timestamp()} | State]}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec length(Timeseries) -> Length when
      Timeseries :: timeseries(),
      Length :: non_neg_integer().
length(#timeseries{events = Events}) ->
    erlang:length(Events).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec info(Timeseries) -> Info when
      Timeseries :: timeseries(),
      Info :: info().
info(#timeseries{} = Timeseries) ->
    length(Timeseries).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec events(Timeseries) -> Events when
      Timeseries :: timeseries(),
      Events :: [event()].
events(#timeseries{events = Events}) ->
    [[Ts, Coord] || {Ts, Coord} <- Events].
