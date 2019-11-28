-module(basic_SUITE).

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

    ?assertEqual({ok, #{}},
                  timeseries_server:info()),

    ?assertEqual({error, unknown_token},
                 timeseries_server:info(<<"example">>)),

    ok = timeseries_server:new(<<"example">>),

    ?assertEqual({ok, #{<<"example">> => 0}},
                 timeseries_server:info()),

    ?assertEqual({ok, 0},
                 timeseries_server:info(<<"example">>)),

    ok = timeseries_server:add(<<"example">>, {1, 112}),
    ok = timeseries_server:add(<<"example">>, {2, 132}),
    ok = timeseries_server:add(<<"example">>, {3, 142}),

    ?assertEqual({ok, #{<<"example">> => 3}},
                 timeseries_server:info()),

    ?assertEqual({ok, 3},
                 timeseries_server:info(<<"example">>)),

    ok = timeseries_server:add(<<"example">>, {4, 144}),
    ok = timeseries_server:add(<<"example">>, {5, 152}),
    ok = timeseries_server:add(<<"example">>, {6, 162}),

    ?assertEqual({ok, #{<<"example">> => 6}},
                 timeseries_server:info()),

    ?assertEqual({ok, 6},
                 timeseries_server:info(<<"example">>)),

    ok.
