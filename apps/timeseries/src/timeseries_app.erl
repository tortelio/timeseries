%%%-----------------------------------------------------------------------------
%%% Copyright (C) 2020 Törteli Olivér Máté <tortelio@yahoo.com>
%%%
%%% All rights reserved.
%%%-----------------------------------------------------------------------------
%%% @doc timeseries public API
%%% @end
%%%-----------------------------------------------------------------------------

-module(timeseries_app).

-behaviour(application).

-include("timeseries.hrl").

%%%=============================================================================
%%% Exports
%%%=============================================================================

%% Application callbacks
-export([start/2,
         stop/1]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Start application.
%% @end
%%------------------------------------------------------------------------------
-spec start(StartType, StartArgs) -> Result when
      StartType :: application:start_type(),
      StartArgs :: term(),
      Result :: {ok, Pid} | {ok, Pid, State} | {error, Reason},
      Pid :: pid(),
      State :: any(),
      Reason :: any().
start(_StartType, _StartArgs) ->
    Port = application:get_env(?APPLICATION, port, 8080),

    PathList =
        [% timeseries interface
         {"/info",              info_http_handler, []},
         {"/download/:token",   download_http_handler, []},
         {"/save/:token",       save_ws_handler, []},
         {"/load/:token",       load_ws_handler, []},

         % monitor
         {"/index.html",        cowboy_static,
          {file, "apps/timeseries/priv/monitor/this/index.html"}},
         {"/TimeseriesClient.html", cowboy_static,
          {file, "apps/timeseries/priv/monitor/this/TimeseriesClient.html"}},
         {"/assets/[...]",      cowboy_static,
          {dir, "apps/timeseries/priv/monitor/this/assets"}},

         % online collectors
         {"/motion-collector",  cowboy_static,
          {file, "apps/timeseries/priv/collectors/sensor.html"}}
        ],

    Dispatch = cowboy_router:compile([{'_', PathList}]),
    {ok, _} = cowboy:start_clear(timeseries_tcp_listener,
                                 [{port, Port}],
                                 #{env => #{dispatch => Dispatch}}),

    timeseries_sup:start_link().

%%------------------------------------------------------------------------------
%% @doc Stop application.
%% @end
%%------------------------------------------------------------------------------
-spec stop(State) -> ok when
      State :: any().
stop(_State) ->
    ok.
