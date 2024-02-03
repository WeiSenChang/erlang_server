%% -*- coding: utf-8 -*-

-module(lib_role).

-include("common.hrl").
-include("db_table.hrl").

%% API
-export([get_data/1, put_data/1]).
-export([
    role_login/1,
    role_logout/1,
    gen_role_show/1,
    role_change_name/2
]).

get_data(RoleId) ->
    lib_db:get_data(?DB_ROLE, RoleId).

put_data(Role) ->
    lib_db:put_data(?DB_ROLE, Role#db_role.role_id, Role).

role_login(RoleId) ->
    Role = get_data(RoleId),
    NewRole = Role#db_role{login_tick = lib_time:unix_time()},
    put_data(NewRole),
    ok.

role_logout(RoleId) ->
    Role = get_data(RoleId),
    NewRole = Role#db_role{logout_tick = lib_time:unix_time()},
    put_data(NewRole),
    ok.

gen_role_show(Role) ->
    #db_role{
        role_id = RoleId,
        name = Name,
        level = Level,
        career = Career
    } = Role,
    #db_role_show{
        role_id = RoleId,
        name = Name,
        level = Level,
        career = Career
    }.

role_change_name(RoleId, Name) ->
    #db_role{name = OldName} = Role = get_data(RoleId),
    case gen_server:call(role_manage_server:get_pid(), {role_change_name, RoleId, OldName, Name}, infinity) of
        success ->
            NewRole = Role#db_role{name = Name},
            put_data(NewRole),
            RoleShow = gen_role_show(NewRole),
            lib_role_manage:put_data(RoleShow);
        _ ->
            ?WARING("role change name fail, name exist. ~w, ~ts", [RoleId, Name])
    end.