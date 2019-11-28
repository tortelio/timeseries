-module(integration_SUITE).
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-compile(export_all).
-compile(nowarn_export_all).

all() ->
    [basics].

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
init_per_testcase(_TestCase, Config) ->
    {ok, _} = application:ensure_all_started(timeseries),
    {ok, _} = application:ensure_all_started(msgpack),
    {ok, _} = application:ensure_all_started(gun),

    % TODO inspect why don't do this automatically
    ok = logger:remove_handler(default),
    ok = logger:add_handlers(kernel),
    % TODO
    ok = logger:set_handler_config(timeseries_file_logger, level, info),

    Config.

%% @doc Clean up after a test case.
-spec end_per_testcase(TestCase, Config) -> ok when
      TestCase :: test_case(),
      Config :: config().
end_per_testcase(_TestCase, _Config) ->
    ok = application:stop(gun),
    ok = application:stop(msgpack),

    [ok = application:stop(App)
     || App <- [timeseries, cowboy, cowlib, ranch]],

    ok.

%% @doc TODO
-spec basics(Config) -> ok when
      Config :: config().
basics(_Config) ->

    % set data
    {ok, Set} = ws_client:connect("localhost", 8080, "/save/my-token-123"),

    ok = ws_client:send(Set, [1, 123]),
    ok = ws_client:send(Set, <<"end">>),
    ok = ws_client:disconnect(Set),

    % wait
    ok = timer:sleep(200),

    % get info
    {ok, Info} = http_client:connect("localhost", 8080),
    ?assertEqual(#{<<"my-token-123">> => 1},
                 http_client:get(Info, "/info")),
    ok = http_client:disconnect(Info),

    % wait
    ok = timer:sleep(200),

    % get data
    {ok, Get} = ws_client:connect("localhost", 8080, "/load/my-token-123"),

    ?assertEqual([[1, 123]], ws_client:take(Get)),

    ok = ws_client:disconnect(Get),

    ok.
