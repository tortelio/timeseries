%%%-----------------------------------------------------------------------------
%%% Copyright (C) 2020 Törteli Olivér Máté <tortelio@yahoo.com>
%%%
%%% All rights reserved.
%%%-----------------------------------------------------------------------------
%%% @doc A cowboy WebSocket handler for loading a given timeseries.
%%% @end
%%%-----------------------------------------------------------------------------

-module(load_ws_handler).
-include("timeseries.hrl").

%%%=============================================================================
%%% Exports
%%%=============================================================================

% Http handler callbacks
-export([init/2,
         websocket_init/1,
         websocket_handle/2,
         websocket_info/2,
         terminate/3]).

%%%=============================================================================
%%% Types
%%%=============================================================================

-record state, {
          token :: timeseries:token(),
          events :: [timeseries:event()] | undefined
         }.

-type state() :: #state{}.

%%%=============================================================================
%%% API functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Cowboy `init' callback
%% @end
%%------------------------------------------------------------------------------
-spec init(Request, Arguments) -> Result when
      Request :: cowboy_req:req(),
      Arguments :: #{},
      Result :: {cowboy_websocket, Request, State, Options} |
                {ok, Request, State},
      State :: state(),
      Options :: cowboy_req:opts().
init(Request, #{}) ->
    Token = cowboy_req:binding(token, Request),
    case timeseries_server:load(Token) of
        {ok, Timeseries} ->
            Events = timeseries:events(Timeseries),
            State = #state{token = Token, events = Events},
            {cowboy_websocket, Request, State, ?WS_OPTIONS};

        {error, unknown_token} ->
            ?LOG_ERROR(#{msg => "Token is unknown",
                         token => Token}),
            State = #state{token = Token},
            {ok, cowboy_req:reply(404, #{}, <<>>, Request), State}
    end.

%%------------------------------------------------------------------------------
%% @doc Cowboy `websocket_init' callback
%% @end
%%------------------------------------------------------------------------------
-spec websocket_init(State) -> {[{binary, Binary}], State} when
      State :: state(),
      Binary :: binary().
websocket_init(#state{token = Token, events = Events} = State) ->
    ?LOG_NOTICE(#{msg => "Load event", token => Token}),
    Commands = [{binary, Binary}
                || Binary <- lists:map(fun jiffy:encode/1, Events)],
    self() ! stop,
    {Commands, State#state{events = Events}}.

%%------------------------------------------------------------------------------
%% @doc Cowboy `websocket_handle' callback
%% @end
%%------------------------------------------------------------------------------
-spec websocket_handle(Message, State) -> Result when
      Message :: ping | pong | {text | binary | ping | pong, binary()},
      State :: state(),
      Result :: {stop, State}.
websocket_handle(Chunk, State) ->
    ?LOG_WARNING(#{msg => "Unknown WebSocket chunk",
                   chunk => Chunk,
                   state => State}),
    {stop, State}.

%%------------------------------------------------------------------------------
%% @doc Cowboy `websocket_info' callback
%% @end
%%------------------------------------------------------------------------------
-spec websocket_info(Info, State) -> Result when
      Info :: any(),
      State :: state(),
      Result :: {stop, State}.
websocket_info(stop, #state{token = Token} = State) ->
    ?LOG_WARNING(#{msg => "Loading ends",
                   token => Token}),
    {stop, State};

websocket_info(Info, State) ->
    ?LOG_WARNING(#{msg => "Unknown WebSocket info",
                   info => Info,
                   state => State}),
    {stop, State}.

%%------------------------------------------------------------------------------
%% @doc Cowboy `terminate' callback
%% @end
%%------------------------------------------------------------------------------
-spec terminate(Reason, PartialRequest, State) -> Result when
      Reason :: any(),
      PartialRequest :: cowboy_req:req(),
      State :: state(),
      Result :: ok.
terminate(Reason, PartialRequest, #state{token = Token}) ->
    ?LOG_ERROR(#{msg => "Terminate",
                 reason => Reason,
                 partial_request => PartialRequest,
                 token => Token}),
    ok.
