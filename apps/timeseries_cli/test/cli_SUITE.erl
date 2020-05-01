-module(cli_SUITE).
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-compile(export_all).
-compile(nowarn_export_all).

all() ->
    [basic_usage].

-define(CMD(__Cmd),
        (fun(Cmd) ->
            lists:flatten(
              io_lib:format("../../../default/bin/timeseries_cli ~s", [Cmd]))
         end)(__Cmd)).

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
    {ok, _} = net_kernel:start(['timeseries@localhost', shortnames]),
    true = erlang:set_cookie(node(), timeseries_cookie),

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
init_per_testcase(basic_usage, Config) ->
    {ok, _} = application:ensure_all_started(timeseries),

    Config.

-spec end_per_testcase(TestCase, Config) -> ok when
      TestCase :: test_case(),
      Config :: config().
end_per_testcase(basic_usage, _Config) ->
    ok = application:stop(timeseries),
    ok.

%% @doc TODO
-spec basic_usage(Config) -> ok when
      Config :: config().
basic_usage(Config) ->
    DataFile = proplists:get_value(data_dir, Config) ++ "/example.data",
    ?assertEqual("Uploading succeeded.\n",
                 os:cmd(?CMD("--upload " ++ DataFile))),

    ok.
