%% -*- coding: utf-8 -*-

-module(lib_login).

-include("common.hrl").
-include("server.hrl").

%% API
-export([create/1, login/2, logout/1]).

create(Name) ->
    ?SERVER_STARTED = lib_cache:get_server_state(?SERVER),
    {ok, [RoleId]} = gen_server:call(role_manage_server:get_pid(), {create_role, Name}),
    RoleId.

login(RoleId, Sid) ->
    ?SERVER_STARTED = lib_cache:get_server_state(?SERVER),
    OnLineMap = lib_cache:get_online_role_map(),
    case maps:get(RoleId, OnLineMap, undefined) of
        undefined -> skip;
        OldSid -> erlang:send(OldSid, {stop, RoleId})
    end,
    logout(RoleId),
    PName = role_server:get_p_name(RoleId),
    server_sup:start_child(PName, role_server, transient, [RoleId, Sid]),
    ok.

logout(RoleId) ->
    PName = role_server:get_p_name(RoleId),
    server_sup:terminate_child(PName),
    server_sup:delete_child(PName),
    ok.