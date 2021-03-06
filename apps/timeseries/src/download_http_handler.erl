%%%-----------------------------------------------------------------------------
%%% Copyright (C) 2020 Törteli Olivér Máté <tortelio@yahoo.com>
%%%
%%% All rights reserved.
%%%-----------------------------------------------------------------------------
%%% @doc A cowboy HTTP handler for downloading a given timeseries.
%%% @end
%%%-----------------------------------------------------------------------------

-module(download_http_handler).
-include("timeseries.hrl").
-behaviour(cowboy_rest).

%%%=============================================================================
%%% Exports
%%%=============================================================================

% Http handler callbacks
-export([init/2]).

% REST callbacks
-export([allowed_methods/2,
         content_types_provided/2,
         resource_exists/2,
         provide_json/2]).

%%%=============================================================================
%%% Types
%%%=============================================================================

-type state() :: #{token => timeseries:token()}.

%%%=============================================================================
%%% API functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Cowboy `init' callback
%% @end
%%------------------------------------------------------------------------------
-spec init(Request, Arguments) -> {cowboy_rest, Request, State} when
      Request :: cowboy_req:req(),
      Arguments :: #{},
      State :: state().
init(Request, #{}) ->
    {cowboy_rest, Request, #{}}.

%%------------------------------------------------------------------------------
%% @doc `allowed_methods'
%% @end
%%------------------------------------------------------------------------------
-spec allowed_methods(Request, State) -> {Methods, Request, State} when
      Request :: cowboy_req:req(),
      State :: state(),
      Methods :: [binary()].
allowed_methods(Request, State) ->
    {[<<"GET">>], Request, State}.

%%------------------------------------------------------------------------------
%% @doc `content_type_provided'
%% @end
%%------------------------------------------------------------------------------
-spec content_types_provided(Request, State) -> Result when
      Result :: {ContentTypes, Request, State},
      Request :: cowboy_req:req(),
      State :: state(),
      ContentTypes :: [{ParsedMime, AcceptCallback}],
      AcceptCallback :: atom(),
      ParsedMime :: {Type, SubType, '*'},
      Type :: binary(),
      SubType :: binary().
content_types_provided(Request, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, provide_json}], Request, State}.

%%------------------------------------------------------------------------------
%% @doc `resource_exists'
%% @end
%%------------------------------------------------------------------------------
-spec resource_exists(Request, State) -> {Exists, Request, State} when
      Request :: cowboy_req:req(),
      State :: state(),
      Exists :: boolean().
resource_exists(Request, State) ->
    Token = cowboy_req:binding(token, Request),
    % TODO replace
    case timeseries_server:info(Token) of
        {ok, _} ->
            {true, Request, State#{token => Token}};
        _ ->
            {false, Request, State}
    end.

%%------------------------------------------------------------------------------
%% @doc `ProvideCallback'
%% @end
%%------------------------------------------------------------------------------
-spec provide_json(Request, State) -> {Body, Request, State} when
      Request :: cowboy_req:req(),
      State :: state(),
      Body :: cowboy_req:resp_body().
provide_json(Request1, #{token := Token} = State) ->
    {ok, Timeseries} = timeseries_server:load(Token),
    Events = timeseries:events(Timeseries),
    Body = jiffy:encode(#{
             <<"token">> => Token,
             <<"events">> => Events
            }),

    Request2 = cowboy_req:set_resp_header(
                 <<"access-control-allow-origin">>, <<"*">>, Request1),
    {Body, Request2, State}.
