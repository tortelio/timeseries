%%%-----------------------------------------------------------------------------
%%% Copyright (C) 2020 Törteli Olivér Máté <tortelio@yahoo.com>
%%%
%%% All rights reserved.
%%%-----------------------------------------------------------------------------
%%% @doc A cowboy HTTP handler for downloading a given timeseries.
%%% @end
%%%-----------------------------------------------------------------------------

-module(download_http_handler).

-include_lib("timeseries.hrl").

%%%=============================================================================
%%% Exports
%%%=============================================================================

% Http handler callbacks
-export([init/2]).

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
      Result :: {ok, Request, noop} | {error, Request}.
init(Request0, []) ->
    ?LOG_NOTICE(#{msg => "Initialize"}),

    case cowboy_req:binding(token, Request0) of
        undefined ->
            ?LOG_ERROR(#{msg => "Missing token"}),
            {error, Request0};
        Token ->
            case timeseries_server:load(Token) of
                {ok, Timeseries} ->
                    Events = timeseries:events(Timeseries),

                    Headers = #{<<"content-type">> => <<"application/json">>,
                                % TODO
                                <<"access-control-allow-origin">> => <<"*">>},
                    Body = jiffy:encode(Events),
                    Request = cowboy_req:reply(200, Headers, Body, Request0),

                    {ok, Request, noop};
                Error ->
                    ?LOG_ERROR(#{msg => "Token can't be loaded",
                                 error => Error}),
                    Request = cowboy_req:reply(404, #{}, <<>>, Request0),
                    {ok, Request, noop}
            end
    end.
