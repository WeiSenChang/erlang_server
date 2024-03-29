%% -*- coding: utf-8 -*-

-module(lib_test_cmd).

-include("common.hrl").
-include("db_table.hrl").

%% API
-export([role_gm/1, role_gm/2, role_gm/3, role_gm/4, role_gm/5]).
-export([rand_show/0]).

role_gm(Gm) ->
    role_gm(Gm, 0).
role_gm(Gm, Par1) ->
    role_gm(Gm, Par1, 0).
role_gm(Gm, Par1, Par2) ->
    role_gm(Gm, Par1, Par2, 0).
role_gm(Gm, Par1, Par2, Par3) ->
    role_gm(Gm, Par1, Par2, Par3, 0).
role_gm("test_time", Secs = [_ | _], _Par2, _Par3, _Par4) ->
    NewSecs =
        case string:tokens(Secs, ",") of
            [Y,Mon,D,H,Min,S] ->
                NowTick = lib_time:unix_time_no_test(),
                ToDataTime = {{lib_types:to_integer(Y), lib_types:to_integer(Mon), lib_types:to_integer(D)},
                    {lib_types:to_integer(H), lib_types:to_integer(Min), lib_types:to_integer(S)}},
                ToTick = lib_time:to_unix_time(ToDataTime),
                Hour = config:server_hour(),
                Minute = config:server_minute(),
                ToTick - NowTick - (Hour * lib_time:hour_second() + Minute * lib_time:minute_second());
            _ ->
                0
        end,
    lib_cache:set_test_secs(NewSecs),
    gen_server:cast(time_server:get_pid(), restart_timer),
    ok;
role_gm("test_time", Secs, _Par2, _Par3, _Par4) ->
    lib_cache:set_test_secs(lib_types:to_integer(Secs)),
    gen_server:cast(time_server:get_pid(), restart_timer),
    ok;

role_gm("create", Num, _, _Par3, _Par4) ->
    StarTick = lib_time:unix_time(),
    ?INFO("create role start"),
    NameMap = lib_cache:get_role_name_map(),
    RoleIds = maps:values(NameMap),
    Names = create(lists:max([0 | RoleIds]), Num),
    lib_role_manage:role_create(Names),
    EndTick = lib_time:unix_time(),
    ?INFO("create role end, use time ~w s", [EndTick - StarTick]);
role_gm(Gm, _Par1, _Par2, _Par3, _Par4) ->
    ?DEBUG("no role gm: ~ts", [Gm]).


%% 内部函数
create(Value, Num) ->
    create(Value, Num, 1, []).
create(Value, Num, Count, Names) ->
    case Num < Count of
        true ->
            lists:reverse(Names);
        false ->
            Name = "wsc" ++ lib_types:to_list(Value + Count),
            create(Value, Num, Count + 1, [Name | Names])
    end.

rand_show() ->
    Max = erlang:map_size(lib_cache:get_role_name_map()),
    ?INFO("~w", [Max]),
    RoleId = rand:uniform(Max),
    ?INFO("~w", [RoleId]),
    lib_role_manage:get_data(RoleId).