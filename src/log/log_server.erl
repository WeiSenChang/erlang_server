%% -*- coding: utf-8 -*-

-module(log_server).

-behaviour(gen_server).

-include("common.hrl").
-include("server.hrl").

%% API
-export([get_pid/0, get_p_name/0, debug_msg/4, info_msg/4, waring_msg/4, error_msg/4]).

%% gen_server callback
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(LOG_WRITE_TIME_OUT, 10 * 1000).

-define(LOG_LEVEL_DEBUG, 0).
-define(LOG_LEVEL_INFO, 1).
-define(LOG_LEVEL_WARING, 2).
-define(LOG_LEVEL_ERROR, 3).

-define(LOG_MAX_SIZE, 10485760).

-define(LOG_PATH, "./log/").

-define(LOG_FD, log_fd).
-define(LOG_LIST, log_list).

-record(log_server_state, {}).

%%%===================================================================
%%% API
%%%===================================================================
get_pid() ->
    erlang:whereis(?MODULE).

get_p_name() ->
    ?MODULE.

debug_msg(Mod, Line, Format, Args) ->
    LogLevel = ?LOG_LEVEL_DEBUG,
    msg(Mod, Line, Format, Args, LogLevel).

info_msg(Mod, Line, Format, Args) ->
    LogLevel = ?LOG_LEVEL_INFO,
    msg(Mod, Line, Format, Args, LogLevel).

waring_msg(Mod, Line, Format, Args) ->
    LogLevel = ?LOG_LEVEL_WARING,
    msg(Mod, Line, Format, Args, LogLevel).

error_msg(Mod, Line, Format, Args) ->
    LogLevel = ?LOG_LEVEL_ERROR,
    msg(Mod, Line, Format, Args, LogLevel).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    erlang:process_flag(trap_exit, true),
    gen_server:cast(self(), db_init),
    lib_cache:set_server_state(?MODULE, ?SERVER_STARTING),
    erlang:send_after(?LOG_WRITE_TIME_OUT, self(), log_write),
    {ok, #log_server_state{}}.

handle_call(_Request, _From, State = #log_server_state{}) ->
    {reply, ok, State}.

handle_cast(db_init, State = #log_server_state{}) ->
    %% do db_init
    open_log(),
    lib_cache:set_server_state(?MODULE, ?SERVER_STARTED),
    NextSec = lib_time:next_hour_time(),
    erlang:send_after(NextSec * 1000, self(), log_add),
    {noreply, State};
handle_cast({msg, Log, LogLevel}, State = #log_server_state{}) ->
    io:format(Log),
    case LogLevel of
        ?LOG_LEVEL_DEBUG ->
            skip;
        _ ->
            LogList = get_log_list(),
            set_log_list([Log | LogList])
    end,
    {noreply, State};
handle_cast(_Request, State = #log_server_state{}) ->
    {noreply, State}.

handle_info(log_add, State = #log_server_state{}) ->
    NextSec = lib_time:next_hour_time(),
    erlang:send_after(NextSec * 1000, self(), log_add),
    write_log(),
    close_log(),
    open_log(),
    {noreply, State};
handle_info(log_write, State = #log_server_state{}) ->
    erlang:send_after(?LOG_WRITE_TIME_OUT, self(), log_write),
    write_log(),
    {noreply, State};
handle_info(_Info, State = #log_server_state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #log_server_state{}) ->
    write_log(),
    close_log(),
    ok.

code_change(_OldVsn, State = #log_server_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
msg(Mod, Line, Format, Args, LogLevel) ->
    Log = gen_log(Mod, Line, Format, Args, LogLevel),
    gen_server:cast(get_pid(), {msg, Log, LogLevel}).

gen_log(Mod, Line, Format, Args, LogLevel) ->
    {_, {H, M, S}} = lib_time:to_date_time(lib_time:unix_time()),
    NewFormat = "[~.2.0w:~.2.0w:~.2.0w][" ++ gen_log_level(LogLevel) ++ "][~w][~w:~w] " ++ Format ++ "~n",
    NewArgs = [H, M, S, self(), Mod, Line | Args],
    io_lib:format(NewFormat, NewArgs).

gen_log_level(?LOG_LEVEL_DEBUG) ->
    "DEBUG";
gen_log_level(?LOG_LEVEL_INFO) ->
    "INFO";
gen_log_level(?LOG_LEVEL_WARING) ->
    "WARING";
gen_log_level(?LOG_LEVEL_ERROR) ->
    "ERROR".

open_log() ->
    FileName = log_name(),
    filelib:ensure_dir(FileName),
    case file:open(FileName, [append]) of
        {ok, Fd} ->
            file:sync(Fd),
            set_log_fd(Fd),
            ok;
        _ -> fail
    end.

log_name() ->
    {{Y, M, D}, {H, _, _}} = lib_time:to_date_time(lib_time:unix_time()),
    ?LOG_PATH ++
        lib_types:to_list(Y) ++ "-" ++
        lib_types:to_list(M) ++ "-" ++
        lib_types:to_list(D) ++ "-" ++
        lib_types:to_list(H) ++ ".log".

write_log() ->
    LogList = get_log_list(),
    set_log_list([]),
    case get_log_fd() of
        undefined -> ok;
        Fd ->
            Name = log_name(),
            case filelib:file_size(Name) >= ?LOG_MAX_SIZE of
                true -> skip;
                false ->
                    case LogList of
                        [_ | _] ->
                            LogStr = string:join(lists:reverse(LogList), ""),
                            io:format(Fd, "~ts", [LogStr]),
                            file:sync(Fd);
                        _ ->
                            skip
                    end
            end
    end.

close_log() ->
    case get_log_fd() of
        undefined -> ok;
        Fd ->
            file:close(Fd),
            file:sync(Fd)
    end.


set_log_fd(Fd) ->
    erlang:put(?LOG_FD, Fd).

get_log_fd() ->
    erlang:get(?LOG_FD).

get_log_list() ->
    case erlang:get(?LOG_LIST) of
        undefined -> [];
        LogList -> LogList
    end.

set_log_list(LogList) ->
    erlang:put(?LOG_LIST, LogList).