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
-export([start_link/0]).

-export([info/0, info/1,
         load/1,
         new/1,
         add/2,
         finish/1]).

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

-type all_info() :: #{timeseriser:token() => timeseries:info()}.
-type container() :: #{timeseries:token() => timeseries:timeseries()}.
-record(state, {timeseries = #{} :: container()}).
-type state() :: #state{}.

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
    gen_server:start_link({local, ?MODULE},
                          ?MODULE,
                          #{},
                          [{timeout, ?TIMEOUT}]).

%%------------------------------------------------------------------------------
%% @doc Get info.
%% @end
%%------------------------------------------------------------------------------
-spec info() -> Result when
      Result :: {ok, AllInfo},
      AllInfo :: all_info().
info() ->
    gen_server:call(?MODULE, info).

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
%% @doc Finish the given timeseries.
%% @end
%%------------------------------------------------------------------------------
-spec finish(Token) -> Result when
      Token :: timeseries:token(),
      Result :: ok | {error, unknown_token}.
finish(Token) ->
    gen_server:call(?MODULE, {finish, Token}).

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
init(_Config = #{}) ->
    {ok, #state{}}.

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
handle_call(info, _, #state{timeseries = All} = State) ->
    ?LOG_NOTICE(#{msg => "Get info"}),

    Info = maps:map(fun(_Token, Timeseries) ->
                            timeseries:info(Timeseries)
                    end, All),

    {reply, {ok, Info}, State};

handle_call({info, Token}, _, #state{timeseries = All} = State) ->
    ?LOG_NOTICE(#{msg => "Get info",
                  token => Token}),

    case maps:find(Token, All) of
        {ok, Timeseries} ->
            {reply, {ok, timeseries:info(Timeseries)}, State};
        error ->
            {reply, {error, unknown_token}, State}
    end;

handle_call({load, Token}, _, #state{timeseries = All} = State) ->
    ?LOG_NOTICE(#{msg => "Load",
                  token => Token}),

    case maps:find(Token, All) of
        {ok, Timeseries} ->
            {reply, {ok, Timeseries}, State};
        error ->
            {reply, {error, unknown_token}, State}
    end;

handle_call({new, Token}, _, #state{timeseries = All1} = State) ->
    ?LOG_NOTICE(#{msg => "New",
                  token => Token}),

    case maps:is_key(Token, All1) of
        false ->
            Timeseries = timeseries:new(Token),
            All2 = All1#{Token => Timeseries},
            {reply, ok, State#state{timeseries = All2}};
        true ->
            {reply, {error, token_already_exist}, State}
    end;

handle_call({add, Token, Event}, _, #state{timeseries = All1} = State) ->
    ?LOG_NOTICE(#{msg => "Add",
                  token => Token,
                  event => Event}),

    case maps:find(Token, All1) of
        {ok, Timeseries1} ->
            Timeseries2 = timeseries:add(Timeseries1, Event),
            All2 = All1#{Token => Timeseries2},
            {reply, ok, State#state{timeseries = All2}};
        error ->
            {reply, {error, unknown_token}, State}
    end;

handle_call({finish, Token}, _, #state{timeseries = All1} = State) ->
    ?LOG_NOTICE(#{msg => "Finish",
                  token => Token}),

    case maps:find(Token, All1) of
        {ok, Timeseries1} ->
            Timeseries2 = timeseries:finish(Timeseries1),
            All2 = All1#{Token => Timeseries2},
            {reply, ok, State#state{timeseries = All2}};
        error ->
            {reply, {error, unknown_token}}
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
