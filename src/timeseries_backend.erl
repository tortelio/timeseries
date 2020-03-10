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
%%% Callbacks
%%%=============================================================================

-callback initialize(Config :: term()) ->
    {ok, State :: term()} |
    {error, Reason :: term()} .

-callback summarize(State1 :: term()) ->
    {{ok, Info :: #{timeseries:token() => timeseries:info()}} |
     {error, Reason :: term()},
     State2 :: term()}.

-callback info(Token :: timeseries:token(),
               State1 :: term()) ->
    {{ok, Info :: timeseries:info()} |
     {error, Reason :: term()},
     State2 :: term()}.

-callback is_available(Token :: timeseries:token(),
                       State1 :: term()) ->
    {{ok, IsAvailable :: boolean()} |
     {error, Reason :: term()},
     State2 :: term()}.

-callback save(Timeseries :: timeseries:timeseries(),
               State1 :: term()) ->
    {ok |
     {error, Reason :: term()},
     State2 :: term()}.

-callback load(Token :: timeseries:token(),
               State1 :: term()) ->
    {{ok, Timeseries :: timseries:timeseries()} |
     {error, Reason :: term()},
     State2 :: term()}.

-callback add(Token :: timeseries:token(),
              Event :: timeseries:event(),
              State1 :: term()) ->
    {ok |
     {error, Reason :: term()},
     State2 :: term()}.
