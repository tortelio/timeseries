%%%-----------------------------------------------------------------------------
%%% Copyright (C) 2020 Törteli Olivér Máté <tortelio@yahoo.com>
%%%
%%% All rights reserved.
%%%-----------------------------------------------------------------------------
%%% @doc A cowboy HTTP handler for downloading a given timeseries.
%%% @end
%%%-----------------------------------------------------------------------------

-module(load_http_handler).

-include_lib("timeseries.hrl").

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

-record state, {token :: timeseries:token()}.

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
      Arguments :: [],
      Result :: {cowboy_websocket, Request, State, Options} | {error, Request},
      State :: state(),
      Options :: cowboy_req:opts().
init(Req, []) ->
    ?LOG_NOTICE(#{msg => "Initialize"}),

    case cowboy_req:binding(token, Req) of
        undefined ->
            ?LOG_ERROR(#{msg => "Missing token"}),
            {error, Req};
        Token ->
            {cowboy_websocket, Req, #state{token = Token}, ?WS_OPTIONS}
    end.

%%------------------------------------------------------------------------------
%% @doc Cowboy `websocket_init' callback
%% @end
%%------------------------------------------------------------------------------
-spec websocket_init(State) -> Result when
      State :: state(),
      Result :: {[Message], State},
      Message :: {binary, Chunk},
      Chunk :: binary().
websocket_init(#state{token = Token} = State) ->
    ?LOG_NOTICE(#{msg => "Initialize WebSocket",
                  state => State}),

    case timeseries_server:load(Token) of
        {ok, Timeseries} ->
            Chunk = msgpack:pack(timeseries:events(Timeseries)),
            {[{binary, Chunk}], State};
        Error ->
            ?LOG_ERROR(#{msg => "Token can't be loaded",
                         error => Error,
                         state => State}),
            Chunk = msgpack:pack(<<"unknown token">>),
            {[{binary, Chunk}], State}
    end.

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
terminate(Reason, PartialRequest, State) ->
    ?LOG_ERROR(#{msg => "Terminate",
                 reason => Reason,
                 partial_request => PartialRequest,
                 state => State}),
    ok.
