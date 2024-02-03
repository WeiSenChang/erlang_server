%% -*- coding: utf-8 -*-

-module(lib_ets).

-include("common.hrl").
-include("ets.hrl").

%% API
-export([ets_init/0, set/2, get/3, delete/2, delete_all/1]).

ets_init() ->
    ?ETS_GLOBAL_CACHE = ets:new(?ETS_GLOBAL_CACHE, ?ETS_OPTS),
    lib_db:ets_init(),
    ok.

get(Ets, Key, Def) ->
    case ets:lookup(Ets, Key) of
        [{Key, Value}] -> Value;
        _ -> Def
    end.

set(Ets, Cache) ->
    ets:insert(Ets, Cache).

delete(Ets, Key) ->
    ets:delete(Ets, Key).

delete_all(Ets) ->
    ets:delete_all_objects(Ets).