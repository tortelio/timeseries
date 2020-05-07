%%%-----------------------------------------------------------------------------
%%% Copyright (C) 2020 Törteli Olivér Máté <tortelio@yahoo.com>
%%%
%%% All rights reserved.
%%%-----------------------------------------------------------------------------
%%% @doc A cowboy HTTP handler for saving a timeseries.
%%% @end
%%%-----------------------------------------------------------------------------
-module(save_ws_handler).

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

-record(state, {token :: timeseries:token()}).

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
init(Request, []) ->
    ?LOG_NOTICE(#{msg => "Initialize"}),

    case cowboy_req:binding(token, Request) of
        undefined ->
            ?LOG_ERROR(#{msg => "Missing token"}),
            {error, Request};
        Token ->
            State = #state{token = Token},
            case timeseries_server:new(Token) of
                ok ->
                    ?LOG_INFO(#{msg => "Saving timeseries",
                                token => Token}),
                    {cowboy_websocket, Request, State, ?WS_OPTIONS};

                {error, token_already_exist} ->
                    ?LOG_ERROR(#{msg => "Token is already exist",
                                 token => Token}),
                    {ok, cowboy_req:reply(409, #{}, <<>>, Request), State}
            end
    end.

%%------------------------------------------------------------------------------
%% @doc Cowboy `websocket_init' callback
%% @end
%%------------------------------------------------------------------------------
-spec websocket_init(State) -> Result when
      State :: state(),
      Result :: {ok, State}.
websocket_init(State) ->
    ?LOG_NOTICE(#{msg => "Initialize SAVE websocket handler",
                  state => State}),
    {ok, State}.

%%------------------------------------------------------------------------------
%% @doc Cowboy `websocket_handle' callback
%% @end
%%------------------------------------------------------------------------------
-spec websocket_handle(Message, State) -> Result when
      Message :: ping | pong | {text | binary | ping | pong, binary()},
      State :: state(),
      Result :: {ok, State} | {stop, State}.
websocket_handle({_, Chunk}, #state{token = Token} = State) ->
    ?LOG_NOTICE(#{msg => "Websocket message received",
                  size => size(Chunk)}),

    try jiffy:decode(Chunk, [return_maps]) of
        #{<<"t">> := _} = Event ->
            ok = timeseries_server:add(Token, Event),
            {ok, State}
    catch
        Class:Exception:_StackTrace ->
            ?LOG_ERROR(#{msg => "Failed message",
                         class => Class,
                         exception => Exception}),
            {stop, State}
    end;

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
terminate(Reason, PartialRequest, State) ->
    ?LOG_ERROR(#{msg => "Terminate",
                 reason => Reason,
                 partial_request => PartialRequest,
                 state => State}),
    ok.
