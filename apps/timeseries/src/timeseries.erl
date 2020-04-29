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
-export([new/1, new/2,
         add/2,
         finish/1]).

% Getters
-export([info/1,
         length/1,
         token/1,
         events/1]).

% Utils
-export([is_valid/1]).

% Types
-export_type([timeseries/0,
              token/0,
              event/0,
              info/0]).

%%%=============================================================================
%%% Types
%%%=============================================================================

-record(timeseries, {token :: token(),
                     events :: [event()],   % this field is containing events in
                                            % reverse order
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

new(Token, Events) ->
    #timeseries{token = Token,
                events = Events,
                state = [{created, erlang:timestamp()}]}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec add(Timeseries1, Event) -> Timeseries2 when
      Timeseries1 :: timeseries(),
      Event :: event(),
      Timeseries2 :: timeseries().
add(#timeseries{events = Events} = Timeseries, Event) ->
    Timeseries#timeseries{events = [Event | Events]}.

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
-spec token(Timeseries) -> Token when
      Timeseries :: timeseries(),
      Token :: token().
token(#timeseries{token = Token}) ->
    Token.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec events(Timeseries) -> Events when
      Timeseries :: timeseries(),
      Events :: [event()].
events(#timeseries{events = Events}) ->
    lists:reverse(Events).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_valid(Any) -> Result when
      Any :: any(),
      Result :: boolean().
is_valid(#timeseries{} = Timeseries) ->
    is_valid(timeseries, Timeseries);
is_valid(Map) when is_map(Map) ->
    is_valid(event, Map);
is_valid(_) ->
    false.

is_valid(timeseries, #timeseries{events = Events}) ->
    lists:all(fun(Event) -> is_valid(event, Event) end, Events);
is_valid(event, #{<<"t">> := _}) ->
    true;
is_valid(_, _) ->
    false.
