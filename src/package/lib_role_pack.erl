%% -*- coding: utf-8 -*-

-module(lib_role_pack).

-include("common.hrl").
-include("db_table.hrl").

%% API
-export([
    get_data/1,
    put_data/1,
    role_login/1
]).

get_data(RoleId) ->
    lib_db:get_data(?DB_ROLE_PACK, RoleId).

put_data(RolePack) ->
    lib_db:put_data(?DB_ROLE_PACK, RolePack#db_role_pack.role_id, RolePack).

role_login(RoleId) ->
    case get_data(RoleId) of
        #db_role_pack{role_id = RoleId} ->
            ok;
        _ ->
            put_data(#db_role_pack{role_id = RoleId})
    end.