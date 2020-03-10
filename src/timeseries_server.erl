%%%-----------------------------------------------------------------------------
%%% Copyright (C) 2020 Törteli Olivér Máté <tortelio@yahoo.com>
%%%
%%% All rights reserved.
%%%-----------------------------------------------------------------------------
%%% @doc Timeseries server
%%% @end
%%%-----------------------------------------------------------------------------

-module(timeseries_server).
-behaviour(gen_server).
-include("timeseries.hrl").

%%%=============================================================================
%%% Exports
%%%=============================================================================

%% API
-export([start_link/0, start_link/1]).

-export([summarize/0,
         info/1,
         save/1,
         load/1,
         new/1,
         add/2]).

-export([load_config/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

%%%=============================================================================
%%% Macros
%%%=============================================================================

-define(TIMEOUT, 5000). % 5 seconds

%%%=============================================================================
%%% Types
%%%=============================================================================

-record(backend, {module :: atom(), state :: term()}).
-type backend() :: #backend{}.

-record(state, {backend :: backend()}).
-type state() :: #state{}.

-type config() :: #{backend => module(),
                    backend_config => map()}.

%%%=============================================================================
%%% API functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Start link.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> Result when
      Result :: {ok, pid()} | ignore | {error, term()}.
start_link() ->
    start_link(#{}).

%%------------------------------------------------------------------------------
%% @doc Start link.
%% @end
%%------------------------------------------------------------------------------
-spec start_link(Config) -> Result when
      Config :: config(),
      Result :: {ok, pid()} | ignore | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE},
                          ?MODULE,
                          Config,
                          [{timeout, ?TIMEOUT}]).

%%------------------------------------------------------------------------------
%% @doc Get info.
%% @end
%%------------------------------------------------------------------------------
-spec summarize() -> Result when
      Result :: {ok, AllInfo},
      AllInfo :: #{timeseriser:token() => timeseries:info()}.
summarize() ->
    gen_server:call(?MODULE, summarize).

%%------------------------------------------------------------------------------
%% @doc Get info about given timeseries.
%% @end
%%------------------------------------------------------------------------------
-spec info(Token) -> Result when
      Token :: timeseries:token(),
      Result :: {ok, Info} | {error, unknown_token},
      Info :: timeseries:info().
info(Token) ->
    gen_server:call(?MODULE, {info, Token}).

%%------------------------------------------------------------------------------
%% @doc Save a given timeseries.
%% @end
%%------------------------------------------------------------------------------
-spec save(Timeseries) -> Result when
      Timeseries :: timeseries:timerseries(),
      Result :: ok | {error, token_already_exist | invalid_timeseries}.
save(Timeseries) ->
    gen_server:call(?MODULE, {save, Timeseries}).

%%------------------------------------------------------------------------------
%% @doc Load a given timeseries.
%% @end
%%------------------------------------------------------------------------------
-spec load(Token) -> Result when
      Token :: timeseries:token(),
      Result :: {ok, Timeseries} | {error, unknonw_token},
      Timeseries :: timeseries:timerseries().
load(Token) ->
    gen_server:call(?MODULE, {load, Token}).

%%------------------------------------------------------------------------------
%% @doc Create an empty timeseries with the given token.
%% @end
%%------------------------------------------------------------------------------
-spec new(Token) -> Result when
      Token :: timeseries:token(),
      Result :: ok | {error, token_already_exist}.
new(Token) ->
    gen_server:call(?MODULE, {new, Token}).

%%------------------------------------------------------------------------------
%% @doc Add event to the given timerseries.
%% @end
%%------------------------------------------------------------------------------
-spec add(Token, Event) -> Result when
      Token :: timeseries:token(),
      Event :: timeseries:event(),
      Result :: ok | {error, unknown_token}.
add(Token, Event) ->
    gen_server:call(?MODULE, {add, Token, Event}).

%%------------------------------------------------------------------------------
%% @doc Load config.
%% @end
%%------------------------------------------------------------------------------
-spec load_config() -> Config when
      Config :: config().
load_config() ->
    Backend = application:get_env(
                ?APPLICATION, backend, timeseries_in_memory_backend),
    BackendConfig = application:get_env(?APPLICATION, backend_config, #{}),
    #{backend => Backend,
      backend_config => BackendConfig}.

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initialize the server.
%% @end
%%------------------------------------------------------------------------------
-spec init(Config) -> Result when
      Config :: #{},
      Result :: {ok, State},
      State :: state().
init(Config) ->
    ?LOG_INFO(#{msg => "Initialize server",
                config => Config}),

    Backend = maps:get(backend, Config, timeseries_in_memory_backend),
    BackendConfig = maps:get(backend_config, Config, #{}),

    case erlang:apply(Backend, initialize, [BackendConfig]) of
        {ok, State} ->
            {ok, #state{backend = #backend{module = Backend, state = State}}};
        {error, Reason} ->
            {error, Reason}
    end.

%%------------------------------------------------------------------------------
%% @doc Handle synchronous requests.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(Request, From, State) -> Result when
      Request :: term(),
      From :: {pid(), term()},
      State :: state(),
      Result :: {reply, Reply, State} | {stop, Reason, State},
      Reply :: term(),
      Reason :: term().
handle_call(summarize, _, #state{backend = Backend1} = State) ->
    ?LOG_NOTICE(#{msg => "Get info"}),

    {Result, Backend2} = backend_apply(Backend1, summarize, []),

    {reply, Result, State#state{backend = Backend2}};

handle_call({info, Token}, _, #state{backend = Backend1} = State) ->
    ?LOG_NOTICE(#{msg => "Get info",
                  token => Token}),

    {Result, Backend2} = backend_apply(Backend1, info, [Token]),

    {reply, Result, State#state{backend = Backend2}};

handle_call({save, Timeseries}, _, #state{backend = Backend1} = State) ->
    ?LOG_NOTICE(#{msg => "Save",
                  token => timeseries:token(Timeseries)}),

    case timeseries:is_valid(Timeseries) of
        true ->
            {Result, Backend2} = backend_apply(Backend1, save, [Timeseries]),
            {reply, Result, State#state{backend = Backend2}};

        false ->
            {reply, {error, invalid_timeseries}, State}
    end;

handle_call({load, Token}, _, #state{backend = Backend1} = State) ->
    ?LOG_NOTICE(#{msg => "Load",
                  token => Token}),

    {Result, Backend2} = backend_apply(Backend1, load, [Token]),
    {reply, Result, State#state{backend = Backend2}};

handle_call({new, Token}, _, #state{backend = Backend1} = State) ->
    ?LOG_NOTICE(#{msg => "New",
                  token => Token}),

    Timeseries = timeseries:new(Token),
    {Result, Backend2} = backend_apply(Backend1, save, [Timeseries]),
    {reply, Result, State#state{backend = Backend2}};

handle_call({add, Token, Event}, _, #state{backend = Backend1} = State) ->
    ?LOG_NOTICE(#{msg => "Add",
                  token => Token,
                  event => Event}),

    case timeseries:is_valid(Event) of
        true ->
            {Result, Backend2} = backend_apply(Backend1, add, [Token, Event]),
            {reply, Result, State#state{backend = Backend2}};

        false ->
            {reply, {error, invalid_event}, State}
    end;

handle_call(Call, From, State) ->
    ?LOG_ERROR(#{msg => "Unhandled call",
                 call => Call,
                 from => From}),
    {stop, {unhandled_call, Call, From}, {error, unknown_call}, State}.

%%------------------------------------------------------------------------------
%% @doc Handle asynchronous requests.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(Message, State) -> Result when
      Message :: term(),
      State :: state(),
      Result :: {noreply, State}.
handle_cast(Message, State) ->
    ?LOG_WARNING(#{msg => "Unhandled cast",
                   message => Message}),
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @doc Handle any other message.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(Info, State) -> Result when
      Info :: term(),
      State :: state(),
      Result :: {noreply, State}.
handle_info(Info, State) ->
    ?LOG_WARNING(#{msg => "Unhandled cast",
                   info => Info}),
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @doc Clean up the gen_server state.
%% @end
%%------------------------------------------------------------------------------
-spec terminate(Reason, State) -> Result when
      Reason :: term(),
      State :: state(),
      Result :: ok.
terminate(Reason, _State) ->
    ?LOG_NOTICE(#{msg => "Terminate",
                  reason => Reason}),
    ok.

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(OldVersion, State, Extra) -> Result when
      OldVersion :: term() | {down, Version},
      Version :: term(),
      State :: state(),
      Extra :: term(),
      Result :: {ok, State}.
code_change(OldVersion, State, Extra) ->
    ?LOG_NOTICE(#{msg => "Code change",
                  old_version => OldVersion,
                  extra => Extra}),
    {ok, State}.

%%%=============================================================================
%%% Private functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Apply backend call
%% @end
%%------------------------------------------------------------------------------
-spec backend_apply(Backend, Function, Arguments) -> Result when
      Backend :: backend(),
      Function :: atom(),
      Arguments :: [term()],
      Result :: {term(), Backend}.
backend_apply(#backend{module = Module, state = State1} = Backend,
              Function,
              Arguments) ->
    {Result, State2} = erlang:apply(Module, Function, Arguments ++ [State1]),
    {Result, Backend#backend{state = State2}}.
