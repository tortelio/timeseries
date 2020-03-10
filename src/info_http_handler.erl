-module(info_http_handler).

-include_lib("timeseries.hrl").

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
      Result :: {ok, Request, noop}.
init(Request0, []) ->
    ?LOG_NOTICE(#{msg => "Initialize"}),

    {ok, Info} = timeseries_server:summarize(),

    ?LOG_NOTICE(#{msg => "Get info",
                  info => Info}),

    Headers = #{<<"content-type">> => <<"application/json">>,
                % TODO
                <<"access-control-allow-origin">> => <<"*">>},
    Body = jiffy:encode(Info),
    Request = cowboy_req:reply(200, Headers, Body, Request0),

    {ok, Request, noop}.
