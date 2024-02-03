%% -*- coding: utf-8 -*-

-module(lib_role_listen).

-include("common.hrl").
-include("db_table.hrl").
-include("role.hrl").

%% API
-export([
    listen_login/2,
    listen_logout/1
]).

listen_login(RoleId, Sid) ->
    gen_server:cast(role_manage_server:get_pid(), {role_login, RoleId, Sid}),
    ?DEBUG("role login ~w", [RoleId]),
    ok.

listen_logout(RoleId) ->
    gen_server:cast(role_manage_server:get_pid(), {role_logout, RoleId}),
    ?DEBUG("role logout ~w", [RoleId]),
    ok.