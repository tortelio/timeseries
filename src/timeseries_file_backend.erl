%%%-----------------------------------------------------------------------------
%%% Copyright (C) 2020 Törteli Olivér Máté <tortelio@yahoo.com>
%%%
%%% All rights reserved.
%%%-----------------------------------------------------------------------------
%%% @doc Timeseries in-memory backend behaviour
%%% @end
%%%-----------------------------------------------------------------------------

-module(timeseries_file_backend).

-include("timeseries.hrl").

-behaviour(timeseries_backend).

%%%=============================================================================
%%% Exports
%%%=============================================================================

-export([initialize/1,
         summarize/1,
         is_available/2,
         add/3,
         info/2,
         save/2,
         load/2]).

%%%=============================================================================
%%% Types
%%%=============================================================================

-type config() :: #{data_dir => string()}.
-type state() :: string().

%%%=============================================================================
%%% API functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec initialize(Config) -> {ok, State} when
      Config :: config(),
      State :: state().
initialize(#{data_dir := DataDir0}) ->
    DataDir = filename:absname(DataDir0),
    ct:pal(DataDir),
    ?LOG_INFO(#{msg => "Initialize",
                data_dir => DataDir}),
    case filelib:is_dir(DataDir) of
        false ->
            ok = filelib:ensure_dir(DataDir),
            ok = file:make_dir(DataDir);
        true ->
            skip
    end,
    {ok, DataDir}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec summarize(State) -> {Result, State} when
      State :: state(),
      Result :: {ok, Info} | {error, Reason},
      Info ::  #{timeseries:token() => timeseries:info()},
      Reason :: term().
summarize(DataDir) ->
    case file:list_dir(DataDir) of
        {ok, Tokens} ->
            Info =
            lists:foldr(fun(Token, Acc) ->
                                {{ok, Info}, DataDir} = info(Token, DataDir),
                                Acc#{erlang:iolist_to_binary(Token) => Info}
                        end, #{}, Tokens),
            {{ok, Info}, DataDir};
        {error, Reason} ->
            {{error, Reason}, DataDir}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_available(Token, State) -> {IsAvailable, State} when
      Token :: timeseries:token(),
      State :: state(),
      IsAvailable :: boolean().
is_available(Token, DataDir) ->
    Path = filename:join([DataDir, Token]),
    {not filelib:is_file(Path), DataDir}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec add(Token, Event, State) -> {ok, State} when
      Token :: timeseries:token(),
      Event :: timeseries:event(),
      State :: state().
add(Token, Event, DataDir) ->
    Path = filename:join([DataDir, Token]),
    {ok, IO} = file:open(Path, [append]),
    ok = write_event(IO, Event),
    ok = file:close(IO),
    {ok, DataDir}.

save(Timeseries, DataDir) ->
    Path = filename:join([DataDir, timeseries:token(Timeseries)]),
    case is_available(Path, DataDir) of
        {true, DataDir} ->
            {ok, IO} = file:open(Path, [append]),
            [ok = write_event(IO, Event)
             || Event <- timeseries:events(Timeseries)],
            ok = file:close(IO),
            {ok, DataDir};
        {false, DataDir} ->
            {{error, token_already_exist}, DataDir}
    end.

load(Token, DataDir) ->
    Path = filename:join([DataDir, Token]),
    case filelib:is_file(Path) of
        true ->
            {ok, IO} = file:open(Path, [read]),
            case read_events(IO) of
                {ok, Events} ->
                    {{ok, timeseries:new(Token, Events)}, DataDir};
                Error ->
                    {Error, DataDir}
            end;

        false ->
            {{error, unknown_token}, DataDir}
    end.

info(Token, DataDir) ->
    Path = filename:join([DataDir, Token]),
    Result = os:cmd(io_lib:format("cat ~s | wc -l", [Path])),
    {Length, []} = string:to_integer(string:trim(Result)),
    {{ok, Length}, DataDir}.

%%%=============================================================================
%%% Internal function
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec write_event(IO, Event) -> ok when
      IO :: file:io_device(),
      Event :: timeseries:event().
write_event(IO, Event) ->
    Bytes = jiffy:encode(Event),
    ok = file:write(IO, Bytes),
    ok = file:write(IO, "\n"),
    ok.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec read_events(IO) -> Result when
      IO :: file:io_device(),
      Result :: {ok, Events} | {error, Reason},
      Events :: [timeseries:event()],
      Reason :: term().
read_events(IO) ->
    read_events(IO, []).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec read_events(IO, Events) -> Result when
      IO :: file:io_device(),
      Result :: {ok, Events} | {error, Reason},
      Events :: [timeseries:event()],
      Reason :: term().
read_events(IO, Events) ->
    case file:read_line(IO) of
        {ok, Line} ->
            Event = jiffy:decode(Line),
            read_events(IO, [Event | Events]);
        eof ->
            {ok, lists:reverse(Events)};
        {error, _} = Error ->
            Error
    end.
