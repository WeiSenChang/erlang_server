%% coding: utf-8
-module(lib_role_manage).
-author("weisenchang").

-include("common.hrl").
-include("db_table.hrl").
-include("role.hrl").

%% API
-export([
    get_data/1,
    put_data/1,
    role_create/1,
    role_login/2,
    role_logout/1,
    role_change_name/1,
    update_on_off_line_map/0
]).

get_data(RoleId) ->
    lib_db:get_data(?DB_ROLE_SHOW, RoleId).

put_data(RoleShow) ->
    lib_db:put_data(?DB_ROLE_SHOW, RoleShow#db_role_show.role_id, RoleShow).

role_create(Names) ->
    RoleNameMap = lib_cache:get_role_name_map(),
    {NewRoleNameMap, RoleIds} = role_create(Names, RoleNameMap),
    lib_cache:set_role_name_map(NewRoleNameMap),
    {ok, RoleIds}.

role_create(Names, RoleNameMap) ->
    role_create(Names, RoleNameMap, []).
role_create([], RoleNameMap, RoleIds) ->
    {RoleNameMap, RoleIds};
role_create([Name | Tail], RoleNameMap, RoleIds) ->
    case maps:is_key(Name, RoleNameMap) of
        false ->
            RoleId = lib_count:get_role_id(),
            Role = #db_role{role_id = RoleId, name = Name},
            lib_db:save(?DB_ROLE, RoleId, Role),
            RoleShow = lib_role:gen_role_show(Role),
            put_data(RoleShow),
            NewRoleNameMap = maps:put(Name, RoleId, RoleNameMap),
            role_create(Tail, NewRoleNameMap, [RoleId | RoleIds]);
        true ->
            {RoleNameMap, RoleIds}
    end.


role_login(RoleId, Sid) ->
    OnOffLineMap = get_on_off_line_map(),
    NewOnOffLineMap = maps:put(RoleId, Sid, OnOffLineMap),
    set_on_off_line_map(NewOnOffLineMap).

role_logout(RoleId) ->
    OnOffLineMap = get_on_off_line_map(),
    NewOnOffLineMap = maps:put(RoleId, ?OFFLINE, OnOffLineMap),
    set_on_off_line_map(NewOnOffLineMap).

role_change_name(Changes) ->
    RoleNameMap = lib_cache:get_role_name_map(),
    case role_change_name(Changes, RoleNameMap) of
        {ok, NewRoleNameMap} ->
            lib_cache:set_role_name_map(NewRoleNameMap),
            success;
        _Other ->
            fail
    end.

role_change_name([], RoleNameMap) ->
    {ok, RoleNameMap};
role_change_name([{RoleId, OldName, Name} | Tail], RoleNameMap) ->
    case maps:get(OldName, RoleNameMap, undefined) of
        RoleId ->
            case maps:is_key(Name, RoleNameMap) of
                false ->
                    NewRoleNameMap0 = maps:remove(OldName, RoleNameMap),
                    NewRoleNameMap1 = maps:put(Name, RoleId, NewRoleNameMap0),
                    role_change_name(Tail, NewRoleNameMap1);
                true ->
                    fail
            end;
        _ ->
            fail
    end.


update_on_off_line_map() ->
    OnOffLineMap = get_on_off_line_map(),
    set_on_off_line_map(#{}),
    OnLineRoleMap = lib_cache:get_online_role_map(),
    OffLineRoleMap = lib_cache:get_offline_role_map(),
    {NewOnLineRoleMap, NewOffLineRoleMap} = maps:fold(
        fun(RoleId, IsOnLine, {Acc0, Acc1}) ->
            case IsOnLine of
                ?OFFLINE ->
                    {maps:remove(RoleId, Acc0), maps:put(RoleId, 1, Acc1)};
                _ ->
                    {maps:put(RoleId, IsOnLine, Acc0), maps:remove(RoleId, Acc1)}
            end
        end, {OnLineRoleMap, OffLineRoleMap}, OnOffLineMap),
    lib_cache:set_online_role_map(NewOnLineRoleMap),
    lib_cache:set_offline_role_map(NewOffLineRoleMap).


get_on_off_line_map() ->
    case erlang:get(on_off_line_map) of
        undefined -> #{};
        OnOffLineMap -> OnOffLineMap
    end.

set_on_off_line_map(OnOffLineMap) ->
    erlang:put(on_off_line_map, OnOffLineMap).