%% -*- coding: utf-8 -*-

-module(server_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Res = server_sup:start_link(),
    lib_server:start(),
    Res.

stop(_State) ->
    ok.
