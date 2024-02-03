%% -*- coding: utf-8 -*-

-module(lib_server).

-include("common.hrl").
-include("server.hrl").

%% API
-export([start/0, stop/0]).

start() ->
    start_as_common(),
    case config:server_type() of
        center ->
            start_as_center();
        game ->
            start_as_game();
        cluster ->
            start_as_cluster()
    end.

start_as_common() ->
    lib_ets:ets_init(),
    lib_db:db_init(),
    ok.

start_as_center() ->
    ok.

start_as_cluster() ->
    ok.

start_as_game() ->
    lib_tcp:server(),
    lib_cache:set_server_state(?SERVER, ?SERVER_STARTING),
    start_as_game(?SERVERS),
    wait_start_end(0),
    ok.
start_as_game([]) ->
    ok;
start_as_game([Server | Tail]) ->
    server_sup:start_child(Server, Server, transient, []),
    start_as_game(Tail).

wait_start_end(Count) ->
    Starteds = get_starteds(Count),
    case Starteds >= length(?SERVERS) of
        true ->
            lib_cache:set_server_state(?SERVER, ?SERVER_STARTED);
        false ->
            timer:sleep(1000),
            wait_start_end(Count + 1)
    end.


get_starteds(Count) ->
    get_starteds(Count, 0, ?SERVERS).
get_starteds(_Count, Starteds, []) ->
    Starteds;
get_starteds(Count, Starteds, [Server | Tail]) ->
    State = lib_cache:get_server_state(Server),
    NewStarteds =
        case State of
            ?SERVER_STARTED ->
                Starteds + 1;
            ?SERVER_STARTING ->
                ?INFO("~w starting, ~w s", [Server, Count]),
                Starteds;
            _ ->
                Starteds
        end,
    get_starteds(Count, NewStarteds, Tail).

stop() ->
    lib_cache:set_server_state(?SERVER, ?SERVER_NO_START),
    OnLineRoleMap = lib_cache:get_online_role_map(),
    online_role_logout(maps:to_list(OnLineRoleMap)),
    ok.

online_role_logout([]) ->
    ok;
online_role_logout([{RoleId, _} | Tail]) ->
    lib_login:logout(RoleId),
    online_role_logout(Tail).