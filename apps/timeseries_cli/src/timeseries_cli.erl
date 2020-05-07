%%%-----------------------------------------------------------------------------
%%% Copyright (C) 2020 Törteli Olivér Máté <tortelio@yahoo.com>
%%%
%%% All rights reserved.
%%%-----------------------------------------------------------------------------
%%% @doc Timeseries CLI escript.
%%% @end
%%%-----------------------------------------------------------------------------

-module(timeseries_cli).

%%%=============================================================================
%%% Exports
%%%=============================================================================

% Escript API
-export([main/1]).

%%%=============================================================================
%%% Macros
%%%=============================================================================

-define(SERVER_NODE, 'timeseries@localhost').

%%%=============================================================================
%%% Escript API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Entry point of the escript.
%% @end
%%------------------------------------------------------------------------------
-spec main(Arguments) -> no_return() when
      Arguments :: [string()].
main(["--upload", DataFile]) ->
    ok = initialize(),

    case file:read_file(DataFile) of
        {ok, Binary} ->
            case jsone:decode(Binary, [{object_format, map}]) of
                #{<<"token">> := Token, <<"events">> := Events} ->
                    Timeseries = call(timeseries, new, [Token, Events]),

                    case call(timeseries_server, save, [Timeseries]) of
                        ok ->
                            log("Uploading succeeded."),
                            erlang:halt(0);

                        {error, Reason} ->
                            log("Saving is failed. Reason: ~p", [Reason]),
                            erlang:halt(1)
                    end;

                _ ->
                    log("Invalid timeseries"),
                    erlang:halt(1)
            end;

        {error, Reason} ->
            log("Reading is failed. Reason: ~p", [Reason]),
            erlang:halt(1)
    end.

%%====================================================================
%% Internal functions
%%====================================================================

%%------------------------------------------------------------------------------
%% @doc Initialize.
%% @end
%%------------------------------------------------------------------------------
-spec initialize() -> ok.
initialize() ->
    % Connect
    {ok, _} = net_kernel:start(['timeseries_cli@localhost', shortnames]),
    true = erlang:set_cookie(node(), timeseries_cookie),
    true = net_kernel:hidden_connect_node(?SERVER_NODE),
    pong = net_adm:ping(?SERVER_NODE),

    % TODO
    {ok, _} = application:ensure_all_started(timeseries_cli),

    ok.

%%------------------------------------------------------------------------------
%% @doc Log.
%% @end
%%------------------------------------------------------------------------------
-spec log(Format) -> ok when
      Format :: io:format().
log(Format) ->
    log(Format, []).

%%------------------------------------------------------------------------------
%% @doc Log.
%% @end
%%------------------------------------------------------------------------------
-spec log(Format, Arguments) -> ok when
      Format :: io:format(),
      Arguments :: [any()].
log(Format, Arguments) ->
    io:format(standard_error, Format ++ "~n", Arguments).

%%------------------------------------------------------------------------------
%% @doc RPC call.
%% @end
%%------------------------------------------------------------------------------
-spec call(Module, Function, Arguments) -> Result when
      Module :: module(),
      Function :: atom(),
      Arguments :: [any()],
      Result :: any().
call(Module, Function, Arguments) ->
    case rpc:call(?SERVER_NODE, Module, Function, Arguments) of
        {badrpc, Reason} ->
            error({badrpc, Reason});
        Result ->
            Result
    end.
