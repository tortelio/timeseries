%%%-----------------------------------------------------------------------------
%%% Copyright (C) 2020 Törteli Olivér Máté <tortelio@yahoo.com>
%%%
%%% All rights reserved.
%%%-----------------------------------------------------------------------------
%%% @doc Timeseries in-memory backend behaviour
%%% @end
%%%-----------------------------------------------------------------------------

-module(timeseries_in_memory_backend).

-behaviour(timeseries_backend).

%%%=============================================================================
%%% Exports
%%%=============================================================================

-export([initialize/1,
         summarize/1,
         is_available/2,
         add/3,
         info/2,
         save/2,
         load/2]).

%%%=============================================================================
%%% Types
%%%=============================================================================

-type config() :: #{}.
-type state() :: #{timeseries:token() => timeseries:timeseries()}.

%%%=============================================================================
%%% API functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initalize
%% @end
%%------------------------------------------------------------------------------
-spec initialize(Config) -> {ok, State} when
      Config :: config(),
      State :: state().
initialize(#{}) ->
    {ok, #{}}.

%%------------------------------------------------------------------------------
%% @doc Summarize
%% @end
%%------------------------------------------------------------------------------
-spec summarize(State) -> {{ok, Summary}, State} when
      State :: state(),
      Summary :: timeseries:summary().
summarize(State) ->
    Info = maps:map(fun(_Token, Timeseries) ->
                            timeseries:info(Timeseries)
                    end, State),
    {{ok, Info}, State}.

%%------------------------------------------------------------------------------
%% @doc Get info for a given token
%% @end
%%------------------------------------------------------------------------------
-spec info(Token, State) -> {{ok, Info} | {error, unknown_token}, State} when
      Token :: timeseries:token(),
      State :: state(),
      Info :: timeseries:info().
info(Token, State) ->
    case maps:find(Token, State) of
        {ok, Timeseries} ->
            {{ok, timeseries:info(Timeseries)}, State};
        error ->
            {{error, unknown_token}, State}
    end.

%%------------------------------------------------------------------------------
%% @doc Save timeseries
%% @end
%%------------------------------------------------------------------------------
-spec save(Timeseries, State) -> {ok | {error, token_already_exist}, State} when
      Timeseries :: timeseries:timeseries(),
      State :: state().
save(Timeseries, State) ->
    Token = timeseries:token(Timeseries),
    case maps:is_key(Token, State) of
        false ->
            {ok, State#{Token => Timeseries}};
        true ->
            {{error, token_already_exist}, State}
    end.

%%------------------------------------------------------------------------------
%% @doc Load timeseries
%% @end
%%------------------------------------------------------------------------------
-spec load(Token, State) -> {{ok, Timeseries} |
                             {error, unknown_token}, State} when
      Token :: timeseries:token(),
      State :: state(),
      Timeseries :: timseries:timeseries().
load(Token, State) ->
    case maps:find(Token, State) of
        {ok, Timeseries} ->
            {{ok, Timeseries}, State};
        error ->
            {{error, unknown_token}, State}
    end.

%%------------------------------------------------------------------------------
%% @doc Return wheter the given token is available.
%% @end
%%------------------------------------------------------------------------------
-spec is_available(Token, State) -> {{ok, IsAvailable}, State} when
      Token :: timeseries:token(),
      State :: state(),
      IsAvailable :: boolean().
is_available(Token, State) ->
    {{ok, not maps:is_key(Token, State)}, State}.

%%------------------------------------------------------------------------------
%% @doc Add an event to an existing timeseries.
%% @end
%%------------------------------------------------------------------------------
-spec add(Token, Event, State) -> {ok | {error, unknown_token}, State} when
      Token :: timeseries:token(),
      Event :: timeseries:event(),
      State :: state().
add(Token, Event, State) ->
    case maps:find(Token, State) of
        {ok, Timeseries1} ->
            Timeseries2 = timeseries:add(Timeseries1, Event),
            {ok, State#{Token => Timeseries2}};

        error ->
            {{error, unknown_token}, State}
    end.
