%%%-----------------------------------------------------------------------------
%%% Copyright (C) 2020 Törteli Olivér Máté <tortelio@yahoo.com>
%%%
%%% All rights reserved.
%%%-----------------------------------------------------------------------------
%%% @doc Timeseries in-memory backend behaviour
%%% @end
%%%-----------------------------------------------------------------------------

-module(timeseries_file_backend).
-behaviour(timeseries_backend).
-include("timeseries.hrl").

%%%=============================================================================
%%% Exports
%%%=============================================================================

-export([initialize/1,
         summarize/1,
         info/2,
         is_available/2,
         save/2,
         load/2,
         add/3]).

%%%=============================================================================
%%% Types
%%%=============================================================================

-type config() :: #{data_dir := string()}.

-record(state, {directory :: string()}).
-type state() :: #state{}.

-type info() :: #{timeseries:token() => non_neg_integer()}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initialize.
%% @end
%%------------------------------------------------------------------------------
-spec initialize(Config) -> {ok, State} when
      Config :: config(),
      State :: state().
initialize(#{data_dir := DataDir}) ->
    ?LOG_INFO(#{msg => "Initialize file backend",
                diretory => DataDir}),
    case filelib:is_dir(DataDir) of
        false ->
            ok = filelib:ensure_dir(DataDir),
            ok = file:make_dir(DataDir);
        _ ->
            skip
    end,

    {ok, #state{directory = DataDir}}.

%%------------------------------------------------------------------------------
%% @doc Summarize.
%% @end
%%------------------------------------------------------------------------------
-spec summarize(State) -> {{ok, Info} | {error, Reason}, State} when
      State :: state(),
      Info :: info(),
      Reason :: any(),
      State :: state().
summarize(State) ->
    case list_tokens(State) of
        {ok, Tokens} ->
            Info = lists:foldr(summarize_fun(State), #{}, Tokens),
            {{ok, Info}, State};
        {error, Reason} ->
            {{error, Reason}, State}
    end.

%%------------------------------------------------------------------------------
%% @doc Info.
%% @end
%%------------------------------------------------------------------------------
-spec info(Token, State) -> {{ok, 1}, State} when
      Token :: timeseries:token(),
      State :: state().
info(Token, State) ->
    Length = get_length(Token, State),
    {{ok, Length}, State}.

%%------------------------------------------------------------------------------
%% @doc Is available.
%% @end
%%------------------------------------------------------------------------------
-spec is_available(Token, State) -> {{ok, IsAvailable}, State} when
      Token :: timeseries:token(),
      State :: state(),
      IsAvailable :: boolean().
is_available(Token, State) ->
    {{ok, not has_token(Token, State)}, State}.

%%------------------------------------------------------------------------------
%% @doc Add.
%% @end
%%------------------------------------------------------------------------------
-spec add(Token, Event, State) -> {ok | {error, unknown_token}, State} when
      Token :: timeseries:token(),
      Event :: timeseries:event(),
      State :: state().
add(Token, Event, State) ->
    case has_token(Token, State) of
        true ->
            Path = path(Token, State),
            {ok, IO} = file:open(Path, [append]),
            ok = append(IO, Event),
            ok = file:close(IO),
            {ok, State};

        false ->
            {{error, unknown_token}, State}
    end.

%%------------------------------------------------------------------------------
%% @doc Save.
%% @end
%%------------------------------------------------------------------------------
-spec save(Timeseries, State) -> {ok | {error, unknown_token}, State} when
      Timeseries :: timeseries:timeseries(),
      State :: state().
save(Timeseries, State) ->
    Token = timeseries:token(Timeseries),
    case has_token(Token, State) of
        false ->
            Path = path(Token, State),
            {ok, IO} = file:open(Path, [append]),
            [ok = append(IO, Event) || Event <- timeseries:events(Timeseries)],
            ok = file:close(IO),
            {ok, State};

        true ->
            {{error, token_already_exist}, State}
    end.

%%------------------------------------------------------------------------------
%% @doc Load.
%% @end
%%------------------------------------------------------------------------------
-spec load(Token, State) -> {{ok, Timeseries} |
                             {error, unknown_token}, State} when
      Token :: timeseries:token(),
      State :: state(),
      Timeseries :: timeseries:timeseries().
load(Token, State) ->
    case has_token(Token, State) of
        true ->
            Path = path(Token, State),
            {ok, IO} = file:open(Path, [read]),
            case read_events(IO) of
                {ok, Events} ->
                    {{ok, timeseries:new(Token, Events)}, State};
                {error, Reason} ->
                    {{error, Reason}, State}
            end;

        false ->
            {{error, unknown_token}, State}
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Path.
%% @end
%%------------------------------------------------------------------------------
-spec path(Token, State) -> Path when
      Token :: timeseries:token(),
      State :: state(),
      Path :: file:filename().
path(Token, #state{directory = Directory}) ->
    filename:join([Directory, Token]).

%%------------------------------------------------------------------------------
%% @doc Summarize function.
%% @end
%%------------------------------------------------------------------------------
-spec summarize_fun(State) -> Function when
      State :: state(),
      Function :: function().
summarize_fun(State) ->
    fun(Token, Info) ->
            Path = path(Token, State),
            Info#{Token => get_length(Path)}
    end.

%%------------------------------------------------------------------------------
%% @doc Get length.
%% @end
%%------------------------------------------------------------------------------
-spec get_length(Token, State) -> Length when
      Token :: timeseries:token(),
      State :: state(),
      Length :: non_neg_integer().
get_length(Token, State) ->
    Path = path(Token, State),
    get_length(Path).

%%------------------------------------------------------------------------------
%% @doc Get length.
%% @end
%%------------------------------------------------------------------------------
-spec get_length(Path) -> Length when
      Path :: file:filename(),
      Length :: non_neg_integer().
get_length(Path) ->
    {Length, []} = string:to_integer(string:trim(os:cmd(io_lib:format("cat ~s | wc -l", [Path])))),
    Length.

%%------------------------------------------------------------------------------
%% @doc Append.
%% @end
%%------------------------------------------------------------------------------
-spec append(IO, Event) -> ok when
      IO :: file:io_device(),
      Event :: timeseries:event().
append(IO, Event) ->
    Bytes = jiffy:encode(Event),
    ok = file:write(IO, Bytes),
    ok = file:write(IO, "\n"),
    ok.

%%------------------------------------------------------------------------------
%% @doc Read events.
%% @end
%%------------------------------------------------------------------------------
-spec read_events(IO) -> {ok, Events} | {error, Reason} when
      IO :: file:io_device(),
      Events :: [timeseries:event()],
      Reason :: any().
read_events(IO) ->
    read_events(IO, []).

%%------------------------------------------------------------------------------
%% @doc Read events.
%% @end
%%------------------------------------------------------------------------------
-spec read_events(IO, Events) -> {ok, Events} | {error, Reason} when
      IO :: file:io_device(),
      Events :: [timeseries:event()],
      Reason :: any().
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

%%------------------------------------------------------------------------------
%% @doc Has token.
%% @end
%%------------------------------------------------------------------------------
-spec has_token(Token, State) -> HasToken when
      Token :: timeseries:token(),
      State :: state(),
      HasToken :: boolean().
has_token(Token, State) ->
    Path = path(Token, State),
    filelib:is_file(Path).

%%------------------------------------------------------------------------------
%% @doc List tokens.
%% @end
%%------------------------------------------------------------------------------
-spec list_tokens(State) -> {ok, Tokens} | {error, Reason} when
      State :: state(),
      Tokens :: [timeseries:token()],
      Reason :: any().
list_tokens(#state{directory = Directory}) ->
    case file:list_dir(Directory) of
        {ok, Filenames} ->
            {ok, lists:map(fun erlang:list_to_binary/1, Filenames)};
        {error, _} = Error ->
            Error
    end.
