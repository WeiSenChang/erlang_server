%% -*- coding: utf-8 -*-

-module(proto_msg_handle).

-include("common.hrl").

%% API
-export([handle/5]).

handle(RoleId, Sid, Msg, RpcId, OpCodeType) ->
    MsgName = element(1, Msg),
    case get_handle_mod(MsgName) of
        undefined -> ?WARING("no handle msg: ~w, ~w, ~w, ~w, ~w", [RoleId, Sid, Msg, RpcId, OpCodeType]);
        Mod -> Mod:handle(RoleId, Sid, Msg, RpcId, OpCodeType)
    end.

get_handle_mod(_) -> undefined.