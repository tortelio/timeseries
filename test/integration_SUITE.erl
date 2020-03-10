-module(integration_SUITE).
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-compile(export_all).
-compile(nowarn_export_all).

all() ->
    [basics,
     basics_with_file_backend
    ].

%%%=============================================================================
%%% Types
%%%=============================================================================

-type config() :: [{atom(), term()}].
-type test_case() :: atom().

%%%-----------------------------------------------------------------------------
%%% Test suite init/end
%%%-----------------------------------------------------------------------------

%% @doc Initialize before the test suite.
-spec init_per_suite(Config) -> Config when
      Config :: config().
init_per_suite(Config) ->
    Config.

%% @doc Clean up after the test suite.
-spec end_per_suite(Config) -> ok when
      Config :: config().
end_per_suite(_Config) ->
    ok.

%%%-----------------------------------------------------------------------------
%%% Test case init/end
%%%-----------------------------------------------------------------------------

%% @doc Initialize before a test case.
-spec init_per_testcase(TestCase, Config) -> Config when
      TestCase :: test_case(),
      Config :: config().
init_per_testcase(basics, Config) ->
    ok = application:set_env(
           [{timeseries,
             [{server,
               #{backend => timeseries_in_memory_backend,
                 backend_config => #{}}
              }]
            }]
          ),

    init_per_testcase(common, Config);

init_per_testcase(basics_with_file_backend, Config) ->
    ok = application:set_env(
           [{timeseries,
             [{server,
               #{backend => timeseries_file_backend,
                 backend_config => #{data_dir => "./timeseries_database"}}
              }]
            }]
          ),

    init_per_testcase(common, Config);

init_per_testcase(common, Config) ->
    {ok, _} = application:ensure_all_started(timeseries),
    {ok, _} = application:ensure_all_started(jiffy),
    {ok, _} = application:ensure_all_started(gun),

    % TODO inspect why don't do this automatically
    try logger:remove_handler(default) catch _:_ -> ok end,
    try logger:add_handlers(kernel) catch _:_ -> ok end,
    % TODO
    logger:set_handler_config(timeseries_file_logger, level, debug),

    Config.

%% @doc Clean up after a test case.
-spec end_per_testcase(TestCase, Config) -> ok when
      TestCase :: test_case(),
      Config :: config().
end_per_testcase(basics, Config) ->
    end_per_testcase(common, Config);

end_per_testcase(basics_with_file_backend, Config) ->
    end_per_testcase(common, Config);

end_per_testcase(common, _Config) ->

    try logger:remove_handler(timeseries_file_logger) catch _:_ -> ok end,

    ok = application:stop(gun),
    ok = application:stop(jiffy),

    [ok = application:stop(App)
     || App <- [timeseries, cowboy, cowlib, ranch]],

    ok.

%% @doc TODO
-spec basics(Config) -> ok when
      Config :: config().
basics(_Config) ->

    % set data
    {ok, Set} = ws_client:connect("0.0.0.0", 8080, "/save/my-token-123"),

    ok = ws_client:send(Set, #{<<"t">> => 1, <<"x">> => 123}),
    ok = ws_client:disconnect(Set),

    % wait
    ok = timer:sleep(200),

    % get info
    {ok, Info} = http_client:connect("0.0.0.0", 8080),
    ?assertEqual(#{<<"my-token-123">> => 1},
                 http_client:get(Info, "/info")),
    ok = http_client:disconnect(Info),

    % wait
    ok = timer:sleep(200),

    % get data
    {ok, Load} = ws_client:connect("0.0.0.0", 8080, "/load/my-token-123"),
    ?assertEqual([#{<<"t">> => 1, <<"x">> => 123}],
                 ws_client:take(Load)),
    ok = ws_client:disconnect(Load),

    % get data
    {ok, Download} = http_client:connect("0.0.0.0", 8080),
    ?assertEqual([#{<<"t">> => 1, <<"x">> => 123}],
                 http_client:get(Download, "/download/my-token-123")),
    ok = http_client:disconnect(Download),

    ok.

%% @doc TODO
-spec basics_with_file_backend(Config) -> ok when
      Config :: config().
basics_with_file_backend(_Config) ->
    ?assertEqual({ok, []}, file:list_dir("./timeseries_database")),

    % set data
    {ok, Set} = ws_client:connect("0.0.0.0", 8080, "/save/my-token-123"),

    ok = ws_client:send(Set, #{<<"t">> => 1, <<"x">> => 123}),
    ?assertEqual({ok, ["my-token-123"]}, file:list_dir("./timeseries_database")),
    ok = ws_client:disconnect(Set),

    % wait
    ok = timer:sleep(200),

    % get info
    {ok, Info} = http_client:connect("0.0.0.0", 8080),
    ?assertEqual(#{<<"my-token-123">> => 1},
                 http_client:get(Info, "/info")),
    ok = http_client:disconnect(Info),

    % wait
    ok = timer:sleep(200),

    % get data
    {ok, Load} = ws_client:connect("0.0.0.0", 8080, "/load/my-token-123"),
    ?assertEqual([#{<<"t">> => 1, <<"x">> => 123}],
                 ws_client:take(Load)),
    ok = ws_client:disconnect(Load),

    % get data
    {ok, Download} = http_client:connect("0.0.0.0", 8080),
    ?assertEqual([#{<<"t">> => 1, <<"x">> => 123}],
                 http_client:get(Download, "/download/my-token-123")),
    ok = http_client:disconnect(Download),

    ok.
