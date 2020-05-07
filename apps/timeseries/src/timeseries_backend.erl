%%%-----------------------------------------------------------------------------
%%% Copyright (C) 2020 Törteli Olivér Máté <tortelio@yahoo.com>
%%%
%%% All rights reserved.
%%%-----------------------------------------------------------------------------
%%% @doc Timeseries backend behaviour
%%% @end
%%%-----------------------------------------------------------------------------

-module(timeseries_backend).

%%%=============================================================================
%%% Types
%%%=============================================================================

-type config() :: any().
-type state() :: any().

%%%=============================================================================
%%% Callbacks
%%%=============================================================================

-callback initialize(Config :: config()) ->
    {ok, State :: state()} | {error, Reason :: any()} .

-callback summarize(State :: state()) ->
    {{ok, Summary :: timeseries:summary()} |
     {error, Reason :: any()},
     State :: state()}.

-callback info(Token :: timeseries:token(),
               State :: state()) ->
    {{ok, Info :: timeseries:info()} |
     {error, Reason :: any()},
     State :: state()}.

-callback is_available(Token :: timeseries:token(),
                       State :: state()) ->
    {{ok, IsAvailable :: boolean()} |
     {error, Reason :: any()},
     State :: state()}.

-callback save(Timeseries :: timeseries:timeseries(),
               State :: state()) ->
    {ok | {error, Reason :: any()}, State :: state()}.

-callback load(Token :: timeseries:token(),
               State :: state()) ->
    {{ok, Timeseries :: timseries:timeseries()} |
     {error, Reason :: any()},
     State :: state()}.

-callback add(Token :: timeseries:token(),
              Event :: timeseries:event(),
              State :: state()) ->
    {ok | {error, Reason :: any()}, State :: state()}.
