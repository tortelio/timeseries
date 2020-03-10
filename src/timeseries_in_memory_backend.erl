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
         finish/2,
         save/2,
         load/2]).

%%%=============================================================================
%%% Types
%%%=============================================================================

-type config() :: #{}.
-type state() :: #{timeseries:token() => timeseries:timeseries()}.

finish(_, State) -> {ok, State}.

%%%=============================================================================
%%% API functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initalize
%% @end
%%------------------------------------------------------------------------------
-spec initialize(Config) -> Result when
      Config :: config(),
      Result :: {ok, State},
      State :: state().
initialize(#{}) ->
    {ok, #{}}.

%%------------------------------------------------------------------------------
%% @doc Summarize
%% @end
%%------------------------------------------------------------------------------
-spec summarize(State) -> Result when
      State :: state(),
      Result :: {ok, Info},
      Info :: #{timeseries:token() => timeseries:info()}.
summarize(State) ->
    Info = maps:map(fun(_Token, Timeseries) ->
                            timeseries:info(Timeseries)
                    end, State),
    {{ok, Info}, State}.

%%------------------------------------------------------------------------------
%% @doc Get info for a given token
%% @end
%%------------------------------------------------------------------------------
-spec info(Token, State) -> Info when
      Token :: timeseries:token(),
      State :: state(),
      Info :: timeseries:info().
info(Token, State) ->
    case maps:find(Token, State) of
        {ok, Timeseries} ->
            {ok, timeseries:info(Timeseries)};
        error ->
            {error, unknown_token}
    end.

-spec save(Timeseries, State) -> Result when
      Timeseries :: timeseries:timeseries(),
      State :: state(),
      Result :: {ok | {error, Reason}, State},
      Reason :: term().
save(Timeseries, State) ->
    Token = timeseries:token(Timeseries),
    case maps:is_key(Token, State) of
        false ->
            {ok, State#{Token => Timeseries}};
        true ->
            {{error, token_already_exist}, State}
    end.

-spec load(Token, State) -> Result when
      Token :: timeseries:token(),
      State :: state(),
      Result :: {ok, Timeseries} | {error, Reason},
      Timeseries :: timseries:timeseries(),
      Reason :: term().
load(Token, State) ->
    case maps:find(Token, State) of
        {ok, Timeseries} ->
            {{ok, Timeseries}, State};
        error ->
            {{error, unknown_token}, State}
    end.

-spec is_available(Token, State) -> Result when
      Token :: timeseries:token(),
      State :: state(),
      Result :: boolean().
is_available(Token, State) ->
    {not maps:is_key(Token, State), State}.

add(Token, Event, State) ->
    case maps:find(Token, State) of
        {ok, Timeseries1} ->
            Timeseries2 = timeseries:add(Timeseries1, Event),
            {ok, State#{Token => Timeseries2}};

        error ->
            {{error, unknown_token}, State}
    end.
