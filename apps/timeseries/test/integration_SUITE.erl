-module(integration_SUITE).
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-compile(export_all).
-compile(nowarn_export_all).

all() ->
    [basics,
     basics_with_file_backend,
     %performance_test_sequential_with_file_backend_via_HTTP,
     performance_test_parallel_with_file_backend_via_HTTP
    ].

-define(REPORT(__Type, __Action, __N, __M, __Time),
        (fun(Type, Action, N, M, Time) ->
             Format = "type = ~p, action = ~p, n = ~p, m = ~p, seconds = ~p~n",
             Line = io_lib:format(Format, [Type, Action, N, M, Time/10000000]),
             file:write_file("/tmp/performance", Line, [append])
         end)(__Type, __Action, __N, __M, __Time)).

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
init_per_testcase(basics, Config1) ->
    Config2 = init_per_testcase(with_in_memory_backend, Config1),
    Config3 = init_per_testcase(common, Config2),
    Config3;

init_per_testcase(basics_with_file_backend, Config1) ->
    Config2 = init_per_testcase(with_file_backend, Config1),
    Config3 = init_per_testcase(common, Config2),
    Config3;

init_per_testcase(performance_test_sequential_with_file_backend_via_HTTP,
                  Config1) ->
    Config2 = init_per_testcase(with_file_backend, Config1),
    Config3 = init_per_testcase(common, Config2),
    Config3;

init_per_testcase(performance_test_parallel_with_file_backend_via_HTTP,
                  Config1) ->
    Config2 = init_per_testcase(with_file_backend, Config1),
    Config3 = init_per_testcase(common, Config2),
    Config3;

init_per_testcase(with_in_memory_backend, Config) ->
    ok = application:set_env(
           [{timeseries,
             [{backend, timeseries_in_memory_backend},
              {backend_config, #{}}]
            }]
          ),

    Config;

init_per_testcase(with_file_backend, Config) ->
    Path = "/tmp/timeseries",

    try os:cmd("rm -rf " ++ Path) catch _:_ -> skip end,

    ok = application:set_env(
           [{timeseries,
             [{backend, timeseries_file_backend},
              {backend_config, #{data_dir => Path}}]
            }]
          ),
    [{file_backend_path, Path} | Config];

init_per_testcase(common, Config) ->
    {ok, _} = application:ensure_all_started(timeseries),
    {ok, _} = application:ensure_all_started(jiffy),
    {ok, _} = application:ensure_all_started(gun),

    % TODO inspect why don't do this automatically
    try logger:remove_handler(default) catch _:_ -> ok end,
    try logger:add_handlers(kernel) catch _:_ -> ok end,
    % TODO
    logger:set_handler_config(timeseries_file_logger, level, warning),

    % Setup server parameters
    Server = #{
      host => "0.0.0.0",
      port => 8080
     },

    [{server, Server} | Config].

%% @doc Clean up after a test case.
-spec end_per_testcase(TestCase, Config) -> ok when
      TestCase :: test_case(),
      Config :: config().
end_per_testcase(basics, Config) ->
    end_per_testcase(common, Config);

end_per_testcase(basics_with_file_backend, Config) ->
    end_per_testcase(common, Config);

end_per_testcase(performance_test_sequential_with_file_backend_via_HTTP,
                 Config) ->
    end_per_testcase(common, Config);

end_per_testcase(performance_test_parallel_with_file_backend_via_HTTP,
                 Config) ->
    end_per_testcase(common, Config);

end_per_testcase(common, _Config) ->

    try logger:remove_handler(timeseries_file_logger) catch _:_ -> skip end,

    ok = application:stop(gun),
    ok = application:stop(jiffy),

    [ok = application:stop(App)
     || App <- [timeseries, cowboy, cowlib, ranch]],

    ok.

%% @doc TODO
-spec basics(Config) -> ok when
      Config :: config().
basics(Config) ->
    Server = proplists:get_value(server, Config),

    Token1 = fixture({token, 1}),
    Events1 = fixture({events, {incremental, 10}}),
    Token2 = fixture({token, 2}),
    Events2 = fixture({events, {incremental, 100}}),

    % Upload timeseries via HTTP
    ok = upload_via_http(Server, Token1, Events1),

    % Save timeseries via WebSocket
    ok = save_via_ws(Server, Token2, Events2),

    % Wait
    ok = timer:sleep(1000),

    % Get summary
    ok = get_summary(Server, #{<<"timeseries1">> => 10,
                               <<"timeseries2">> => 100}),

    % Load "timeseries1" via WebSocket
    ok = load_via_ws(Server, Token1, Events1),

    % Load "timeseries2" via WebSocket
    ok = load_via_ws(Server, Token2, Events2),

    % Download "timeseries1" va HTTP
    ok = download_via_http(Server, Token1, Events1),

    % Download "timeseries2" va HTTP
    ok = download_via_http(Server, Token2, Events2),

    ok.

%% @doc TODO
-spec basics_with_file_backend(Config) -> ok when
      Config :: config().
basics_with_file_backend(Config) ->
    Server = proplists:get_value(server, Config),
    FileBackendPath = proplists:get_value(file_backend_path, Config),

    Token1 = fixture({token, 1}),
    Events1 = fixture({events, {incremental, 10}}),
    Token2 = fixture({token, 2}),
    Events2 = fixture({events, {incremental, 100}}),

    ok = check_created_files(FileBackendPath, []),

    % Upload timeseries via HTTP
    ok = upload_via_http(Server, Token1, Events1),
    ok = check_created_files(FileBackendPath, ["timeseries1"]),

    % Save timeseries via WebSocket
    ok = save_via_ws(Server, Token2, Events2),
    ok = check_created_files(FileBackendPath, ["timeseries1", "timeseries2"]),

    % Wait
    ok = timer:sleep(1000),

    % Get summary
    ok = get_summary(Server, #{<<"timeseries1">> => 10,
                               <<"timeseries2">> => 100}),

    % Load "timeseries1" via WebSocket
    ok = load_via_ws(Server, Token1, Events1),

    % Load "timeseries2" via WebSocket
    ok = load_via_ws(Server, Token2, Events2),

    % Download "timeseries1" va HTTP
    ok = download_via_http(Server, Token1, Events1),

    % Download "timeseries2" va HTTP
    ok = download_via_http(Server, Token2, Events2),

    ok.

performance_test_sequential_with_file_backend_via_HTTP(Config1) ->
    [try
         try os:cmd("rm -rf /tmp/timeseries/*") catch _:_ -> skip end,
         Config2 = [{no_timeseries, N}, {no_events, M} | Config1],
         ok = do_performance_test_sequential_with_file_backend_via_HTTP(Config2)
     catch
         _:_ ->
             skip
     end
     || N <- [10, 20, 30, 50],
        M <- [100, 500, 1000, 5000, 10000, 20000]],

    ok.

do_performance_test_sequential_with_file_backend_via_HTTP(Config) ->
    Server = proplists:get_value(server, Config),
    NOTimeseries = proplists:get_value(no_timeseries, Config),
    NOEvents = proplists:get_value(no_events, Config),

    % Batch upload
    {UploadTime, ok} = timer:tc(fun sequential_upload_via_http/3,
                                [Server, NOTimeseries, NOEvents]),

    ?REPORT(sequential, upload, NOTimeseries, NOEvents, UploadTime),

    % Batch download
    {DownloadTime, ok} = timer:tc(fun sequential_download_via_http/3,
                                [Server, NOTimeseries, NOEvents]),

    ?REPORT(sequential, download, NOTimeseries, NOEvents, DownloadTime),

    ok.

performance_test_parallel_with_file_backend_via_HTTP(Config1) ->
    [try
         try os:cmd("rm -rf /tmp/timeseries/*") catch _:_ -> skip end,
         Config2 = [{no_timeseries, N}, {no_events, M} | Config1],
         ok = do_performance_test_parallel_with_file_backend_via_HTTP(Config2)
     catch
         _:_ ->
             skip
     end
     || N <- [10, 20, 30, 50],
        M <- [100, 500, 1000, 5000, 10000, 20000]],

    ok.

do_performance_test_parallel_with_file_backend_via_HTTP(Config) ->
    Server = proplists:get_value(server, Config),
    NOTimeseries = proplists:get_value(no_timeseries, Config),
    NOEvents = proplists:get_value(no_events, Config),

    % Batch upload
    {UploadTime, ok} = timer:tc(fun parallel_upload_via_http/3,
                                [Server, NOTimeseries, NOEvents]),

    ?REPORT(parallel, upload, NOTimeseries, NOEvents, UploadTime),

    % Batch download
    {DownloadTime, ok} = timer:tc(fun parallel_download_via_http/3,
                                  [Server, NOTimeseries, NOEvents]),

    ?REPORT(parallel, download, NOTimeseries, NOEvents, DownloadTime),

    ok.

get_summary(#{host := Host, port := Port}, ExpectedSummary) ->
    {ok, Info} = http_client:connect(Host, Port),
    ?assertEqual(ExpectedSummary, http_client:get(Info, "/summary")),
    ok = http_client:disconnect(Info),
    ok.

upload_via_http(#{host := Host, port := Port}, Token, Events) ->
    {ok, Connection} = http_client:connect(Host, Port),
    Path = "/upload/" ++ binary_to_list(Token),
    Data = #{<<"token">> => Token, <<"events">> => Events},
    ?assertEqual(#{<<"result">> => <<"ok">>},
                 http_client:post(Connection, Path, Data)),
    ok = http_client:disconnect(Connection),
    ok.

download_via_http(#{host := Host, port := Port}, Token, ExpectedEvents) ->
    {ok, Connection} = http_client:connect(Host, Port),
    Path = "/download/" ++ binary_to_list(Token),
    ?assertEqual(#{<<"token">> => Token, <<"events">> => ExpectedEvents},
                 http_client:get(Connection, Path)),
    ok = http_client:disconnect(Connection),
    ok.

sequential_upload_via_http(Server, N, M) ->
    Ids = lists:seq(1, N),
    [begin
         Token = fixture({token, Id}),
         Events = fixture({events, {incremental, M}}),
         ok = upload_via_http(Server, Token, Events)
     end || Id <- Ids],
    ok.

sequential_download_via_http(Server, N, M) ->
    Ids = lists:seq(1, N),
    [begin
         Token = fixture({token, Id}),
         Events = fixture({events, {incremental, M}}),
         ok = download_via_http(Server, Token, Events)
     end || Id <- Ids],
    ok.

parallel_upload_via_http(Server, N, M) ->
    Ids = lists:seq(1, N),
    [do_spawn(Id, upload_via_http_fun(Server, Id, M)) || Id <- Ids],
    ?assert(lists:all(fun({_Id, Result}) -> Result =:= ok end,
                      wait_responses(Ids))),
    ok.

upload_via_http_fun(Server, Id, M) ->
    fun() ->
            Token = fixture({token, Id}),
            Events = fixture({events, {incremental, M}}),
            ok = upload_via_http(Server, Token, Events)
    end.

download_via_http_fun(Server, Id, M) ->
    fun() ->
            Token = fixture({token, Id}),
            Events = fixture({events, {incremental, M}}),
            ok = download_via_http(Server, Token, Events)
    end.

do_spawn(Id, F) ->
    Pid = self(),
    spawn_link(fun() ->
                       io:format("spawned: (~p) #~p~n", [Pid, Id]),
                       Pid ! {Id, F()}
               end).

parallel_download_via_http(Server, N, M) ->
    Ids = lists:seq(1, N),
    [do_spawn(Id, download_via_http_fun(Server, Id, M)) || Id <- Ids],
    ?assert(lists:all(fun({_Id, Result}) -> Result =:= ok end,
                      wait_responses(Ids))),
    ok.

wait_responses(Ids) ->
    wait_responses(Ids, []).

wait_responses([], Acc) ->
    Acc;

wait_responses(Ids, Acc) ->
    receive
        {Id, Result} ->
            wait_responses(Ids -- [Id], [{Id, Result} | Acc])
    after
        10000 ->
            error({timeout, {missing_results, Ids}})
    end.

save_via_ws(#{host := Host, port := Port}, Token, Events) ->
    Path = "/save/" ++ binary_to_list(Token),
    {ok, Connection} = ws_client:connect(Host, Port, Path),
    [ok = ws_client:send(Connection, Event) || Event <- Events],
    ok = ws_client:disconnect(Connection),
    ok.

load_via_ws(#{host := Host, port := Port}, Token, ExpectedEvents) ->
    Path = "/load/" ++ binary_to_list(Token),
    {ok, Load} = ws_client:connect(Host, Port, Path),
    [?assertEqual(ExpectedEvent, ws_client:take(Load))
     || ExpectedEvent <- ExpectedEvents],
    ok = ws_client:disconnect(Load),
    ok.

check_created_files(Path, ExpectedFiles) ->
    Result = file:list_dir(Path),
    ?assertMatch({ok, _}, Result),
    {ok, Files} = Result,
    ?assertEqual(lists:sort(ExpectedFiles), lists:sort(Files)),
    ok.

fixture({token, I}) ->
    <<"timeseries", (erlang:integer_to_binary(I))/binary>>;

fixture({events, {incremental, N}}) ->
    [#{<<"t">> => I, <<"x">> => I} || I <- lists:seq(1, N)].
