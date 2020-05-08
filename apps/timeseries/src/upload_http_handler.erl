%%%-----------------------------------------------------------------------------
%%% Copyright (C) 2020 Törteli Olivér Máté <tortelio@yahoo.com>
%%%
%%% All rights reserved.
%%%-----------------------------------------------------------------------------
%%% @doc A cowboy HTTP handler for uploading a given timeseries.
%%% @end
%%%-----------------------------------------------------------------------------

-module(upload_http_handler).
-include("timeseries.hrl").
-behaviour(cowboy_rest).

%%%=============================================================================
%%% Exports
%%%=============================================================================

% Http handler callbacks
-export([init/2]).

% REST callbacks
-export([allowed_methods/2,
         content_types_accepted/2,
         accept_json/2]).

%%%=============================================================================
%%% Types
%%%=============================================================================

-type state() :: #{}.

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
    {[<<"POST">>], Request, State}.

%%------------------------------------------------------------------------------
%% @doc `content_type_accepted'
%% @end
%%------------------------------------------------------------------------------
-spec content_types_accepted(Request, State) -> Result when
      Result :: {ContentTypes, Request, State},
      Request :: cowboy_req:req(),
      State :: state(),
      ContentTypes :: [{ParsedMime, AcceptCallback}],
      AcceptCallback :: atom(),
      ParsedMime :: {Type, SubType, '*'},
      Type :: binary(),
      SubType :: binary().
content_types_accepted(Request, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, accept_json}], Request, State}.

%%------------------------------------------------------------------------------
%% @doc `AcceptCallback'
%% @end
%%------------------------------------------------------------------------------
-spec accept_json(Request, State) -> {true | false, Request, State} when
      Request :: cowboy_req:req(),
      State :: state().
accept_json(Request1, State) ->
    Token = cowboy_req:binding(token, Request1),
    {ok, Body, Request2} = read_body(Request1),

    case jiffy:decode(Body, [return_maps]) of
        #{<<"token">> := Token,
          <<"events">> := Events} ->
            Timeseries = timeseries:new(Token, Events),
            case timeseries_server:save(Timeseries) of
                ok ->
                    Request3 = cowboy_req:set_resp_header(
                                 <<"content-type">>,
                                 <<"application/json">>,
                                 Request2),
                    Request4 = cowboy_req:set_resp_body(
                                 jiffy:encode(#{<<"result">> => <<"ok">>}),
                                 Request3),

                    {true, Request4, State};

                {error, token_already_exist} ->
                    {false, Request1, State};

                {error, invalid_timeseries} ->
                    {false, Request1, State}
            end;
        Data ->
            ?LOG_WARNING(#{msg => "Invalid input data",
                           data => Data}),
            {false, Request1, State}
    end.

read_body(Request) ->
    read_body(Request, <<>>).

read_body(Request1, Acc) ->
    case cowboy_req:read_body(Request1) of
        {ok, Data, Request2} ->
            {ok, <<Acc/binary, Data/binary>>, Request2};
        {more, Data, Request2} ->
            read_body(Request2, <<Acc/binary, Data/binary>>)
    end.
