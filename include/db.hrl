%% -*- coding: utf-8 -*-

-ifndef('db_HRL').
-define('db_HRL', true).

-define(INT, int).
-define(FLOAT, float).
-define(STRING, string).
-define(LIST, list).
-define(MAP, map).

-define(ROLE_TAB, "role").
-define(SYS_TAB, "sys").

-define(SAVE_SECS, 300).

-define(CACHE_SAVE, 1).
-define(CACHE_NO_SAVE, 0).

-record(table, {key, type, secs = 0, fields = []}).
-record(field, {name, type, sub_type, value}).

-define(DATA_KEY(Tab, Key), {Tab, Key}).
-define(UN_DATA_KEY(DataKey), #{tab => element(1, DataKey), key => element(2, DataKey)}).
-define(SAVE_DATA_KEY_MAP, save_data_key_map).

-endif.