%% -*- coding: utf-8 -*-

-module(lib_test_cmd).

-include("common.hrl").
-include("db_table.hrl").

%% API
-export([role_gm/1, role_gm/2, role_gm/3, role_gm/4, role_gm/5]).
-export([rand_show/0, insert_item/3, test_tcp/1]).

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
                ToTick - NowTick;
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
role_gm("change_name", _Par1, _Par2, _Par3, _Par4) ->
    StarTick = lib_time:unix_time(),
    Roles = lib_db:load_all(?DB_ROLE),
    {NewRoles, Changes} = change_name(Roles),
    case lib_role_manage:role_change_name(Changes) of
        success -> do_change_name(NewRoles);
        _ -> skip
    end,
    EndTick = lib_time:unix_time(),
    ?DEBUG("change name end, use time ~w s", [EndTick - StarTick]);
role_gm("all_role_login", _Par1, _Par2, _Par3, _Par4) ->
    StarTick = lib_time:unix_time(),
    OffLineMap = lib_cache:get_offline_role_map(),
    RoleIds = maps:keys(OffLineMap),
    LoginIds = lists:sublist(RoleIds, 100000),
    lists:foreach(
        fun(LoginId) ->
            PName = role_server:get_p_name(LoginId),
            server_sup:start_child(PName, role_server, transient, [LoginId])
        end, LoginIds),
    EndTick = lib_time:unix_time(),
    ?INFO("role login end, use time ~w s", [EndTick - StarTick]);
role_gm("all_role_logout", _Par1, _Par2, _Par3, _Par4) ->
    StarTick = lib_time:unix_time(),
    OnLineMap = lib_cache:get_online_role_map(),
    ?DEBUG("~w", [map_size(OnLineMap)]),
    all_role_logout(maps:to_list(OnLineMap)),
    EndTick = lib_time:unix_time(),
    ?DEBUG("role logout end, use time ~w s", [EndTick - StarTick]);


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

change_name(Roles) ->
    change_name(Roles, [], []).
change_name([], Roles, Changes) ->
    {Roles, Changes};
change_name([Role | Tail], Roles, Changes) ->
    #db_role{role_id = RoleId, name = OldName} = Role,
    Name = "test" ++ lib_types:to_list(RoleId),
    NewRole = Role#db_role{name = Name},
    change_name(Tail, [NewRole |Roles], [{RoleId, OldName, Name} | Changes]).

do_change_name([]) ->
    ok;
do_change_name([Role | Tail]) ->
    lib_role:set_data(Role),
    RoleShow = lib_role:gen_role_show(Role),
    lib_role_manage:set_data(RoleShow),
    do_change_name(Tail).

all_role_logout([]) ->
    ok;
all_role_logout([{RoleId, _} | Tail]) ->
    lib_login:logout(RoleId),
    all_role_logout(Tail).

rand_show() ->
    Max = map_size(lib_cache:get_role_name_map()),
    ?INFO("~w", [Max]),
    RoleId = rand:uniform(Max),
    ?INFO("~w", [RoleId]),
    lib_role_manage:get_data(RoleId).

insert_item(RoleId, ItemId, Amount) ->
    #db_role_pack{role_id = RoleId, package_map = PackageMap} = RolePack = lib_db:get(?DB_ROLE_PACK, RoleId),
    Type = get_type(ItemId),
    #r_package{item_map = ItemMap} = Package = maps:get(Type, PackageMap, #r_package{type = Type}),
    Item = maps:get(ItemId, ItemMap, #r_item{item_id = ItemId}),
    NewItem = Item#r_item{amount = Amount + Item#r_item.amount},
    NewItemMap = maps:put(ItemId, NewItem, ItemMap),
    NewPackage = Package#r_package{item_map = NewItemMap},
    NewPackageMap = maps:put(Type, NewPackage, PackageMap),
    NewRolePack = RolePack#db_role_pack{package_map = NewPackageMap},
    lib_db:set(?DB_ROLE_PACK, RoleId, NewRolePack).

get_type(ItemId) -> ItemId div 1000 + 1.

test_tcp(RoleId) ->
    gen_server:cast(role_server:get_pid(RoleId), test_tcp).