%% -*- coding: utf-8 -*-

-module(main).

-include("common.hrl").
-include("server.hrl").

%% API
-export([start/0, stop/0]).

start() ->
    config:reload(),
    application:start(server),
    io:format("game start~n").


stop() ->
    lib_server:stop(),
    application:stop(server),
    io:format("game stop~n"),
    init:stop().