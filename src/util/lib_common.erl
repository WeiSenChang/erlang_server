%% -*- coding: utf-8 -*-

-module(lib_common).

-include("common.hrl").

%% API
-export([
    to_big_int/2
]).

to_big_int(B, S) ->
    B + S.