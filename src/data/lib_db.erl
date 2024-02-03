%% -*- coding: utf-8 -*-

-module(lib_db).

-include("common.hrl").
-include("db_table.hrl").
-include("ets.hrl").
-include("db.hrl").

%% API
%% init func
-export([db_init/0, ets_init/0]).
%% db func
-export([find_count/1, all_keys/1, load_all/1, load_one/2, save/3, save/0]).
%% process func
-export([get_data/2, put_data/3]).

db_init() ->
    case mnesia:system_info(use_dir) of
        false -> mnesia:create_schema([node()]);
        true -> ignore
    end,
    mnesia:start(),
    create_tabs().

ets_init() ->
    Tabs = db_table:role_tabs() ++ db_table:sys_tabs(),
    ets_init(Tabs).
ets_init([]) ->
    ok;
ets_init([Tab | Tail]) ->
    Tab = ets:new(Tab, ?ETS_OPTS),
    ets_init(Tail).

find_count(Key) ->
    Fun = fun() -> do_find_count(Key) end,
    {atomic, Value} = mnesia:transaction(Fun),
    Value.

all_keys(?DB_COUNT) ->
    Fun = fun() -> mnesia:all_keys(?DB_COUNT) end,
    {atomic, AllKeys} = mnesia:transaction(Fun),
    AllKeys;
all_keys(Tab) ->
    Field = get_key_field(Tab),
    Fun = fun() -> mnesia:all_keys(Tab) end,
    {atomic, AllKeys} = mnesia:transaction(Fun),
    [get_bson_value(Field, Bson) || Bson <- AllKeys].

load_all(Tab) ->
    Fun0 = fun({_, _, Bson}, Acc) -> [bson_to_record(Tab, Bson) | Acc] end,
    Fun1 = fun() -> mnesia:foldl(Fun0, [], Tab) end,
    case mnesia:transaction(Fun1) of
        {atomic, BsonList} -> BsonList;
        _ -> []
    end.

load_one(Tab, Key) ->
    KeyBson = key_to_bson(Tab, Key),
    Fun = fun() -> mnesia:read({Tab, KeyBson}) end,
    case mnesia:transaction(Fun) of
        {atomic, [{Tab, KeyBson, Bson}]} -> bson_to_record(Tab, Bson);
        _ -> undefined
    end.

save(Tab, Key, Value) ->
    KeyBson = key_to_bson(Tab, Key),
    Bson = record_to_bson(Value),
    Fun = fun() -> mnesia:write({Tab, KeyBson, Bson}) end,
    mnesia:transaction(Fun).

save() ->
    SaveMap = get_save_data_key_map(),
    put_save_data_key_map(#{}),
    maps:fold(
        fun(DataKey, _, Acc) ->
            #{tab := Tab, key := Key} = ?UN_DATA_KEY(DataKey),
            Data = get_data(Tab, Key),
            save(Tab, Key, Data),
            Acc
        end, ok, SaveMap).

get_data(Tab, Key) ->
    case erlang:get(?DATA_KEY(Tab, Key)) of
        undefined -> undefined;
        Bson -> bson_to_record(Tab, Bson)
    end.

put_data(Tab, Key, Value) ->
    Bson = record_to_bson(Value),
    case erlang:put(?DATA_KEY(Tab, Key), Bson) of
        undefined -> ignore;
        Bson -> ignore;
        _OldBson ->
            SaveDataKeyMap = get_save_data_key_map(),
            put_save_data_key_map(maps:put(?DATA_KEY(Tab, Key), 1, SaveDataKeyMap))
    end.

%% 内部函数
%%%%%%%%%%%%%%%%%%%%%%
%% 数据结构转换
record_to_bson(Record)->
    Fields = db_table:get_fields(Record),
    record_to_bson(#{}, Fields).
record_to_bson(Bson, []) ->
    Bson;
record_to_bson(Bson, [Field | Tail]) ->
    NewBson = set_bson_value(Field, Bson),
    record_to_bson(NewBson, Tail).

set_bson_value(Field, Bson) ->
    #field{name = Name, type = Type, sub_type = SubType, value = Value} = Field,
    set_bson_value(Type, SubType, Name, Value, Bson).
set_bson_value(?INT, _, Name, Value, Bson) ->
    maps:put(lib_types:to_binary(Name), lib_types:to_integer(Value), Bson);
set_bson_value(?FLOAT, _, Name, Value, Bson) ->
    maps:put(lib_types:to_binary(Name), lib_types:to_float(Value), Bson);
set_bson_value(?STRING, _, Name, Value, Bson) ->
    maps:put(lib_types:to_binary(Name), lib_types:to_binary(Value), Bson);
set_bson_value(?LIST, SubType, Name, Value, Bson) ->
    case SubType of
        ?INT -> maps:put(lib_types:to_binary(Name), Value, Bson);
        ?FLOAT -> maps:put(lib_types:to_binary(Name), Value, Bson);
        ?STRING -> maps:put(lib_types:to_binary(Name), [lib_types:to_binary(V) || V <- Value], Bson);
        _ -> maps:put(lib_types:to_binary(Name), [record_to_bson(V) || V <- Value], Bson)
    end;
set_bson_value(?MAP, SubType, Name, Value, Bson) ->
    set_bson_value(?LIST, SubType, Name, maps:values(Value), Bson);
set_bson_value(_, _, Name, Value, Bson) ->
    maps:put(lib_types:to_binary(Name), record_to_bson(Value), Bson).

bson_to_record(Tab, Bson) ->
    FieldMap = db_table:get_field_map(Tab),
    NewFieldMap = update_field_map(Bson, FieldMap),
    db_table:field_map_to_record(Tab, NewFieldMap).
update_field_map(Bson, FieldMap) ->
    maps:fold(
        fun(Name, Field, Acc) ->
            Value = get_bson_value(Field, Bson),
            NewField = Field#field{value = Value},
            maps:put(Name, NewField, Acc)
        end, #{}, FieldMap).

get_bson_value(Field, Bson) ->
    #field{name = Name, type = Type, sub_type = SubType} = Field,
    get_bson_value(Type, SubType, Name, Bson).
get_bson_value(?INT, _, Name, Bson) ->
    lib_types:to_integer(maps:get(lib_types:to_binary(Name), Bson, 0));
get_bson_value(?FLOAT, _, Name, Bson) ->
    lib_types:to_float(maps:get(lib_types:to_binary(Name), Bson, 0.0));
get_bson_value(?STRING, _, Name, Bson) ->
    lib_types:to_list(maps:get(lib_types:to_binary(Name), Bson, ""));
get_bson_value(?LIST, SubType, Name, Bson) ->
    case SubType of
        ?INT -> [lib_types:to_integer(V) || V <- lib_types:to_list(maps:get(lib_types:to_binary(Name), Bson, []))];
        ?FLOAT -> [lib_types:to_float(V) || V <- lib_types:to_list(maps:get(lib_types:to_binary(Name), Bson, []))];
        ?STRING -> [lib_types:to_list(V) || V <- lib_types:to_list(maps:get(lib_types:to_binary(Name), Bson, []))];
        _ -> [bson_to_record(V, SubType) || V <- lib_types:to_list(maps:get(lib_types:to_binary(Name), Bson, []))]
    end;
get_bson_value(?MAP, SubType, Name, Bson) ->
    KeyName = db_table:get_map_key(SubType),
    lists:foldl(
        fun(B, Acc) ->
            Value = bson_to_record(B, SubType),
            Key = maps:get(lib_types:to_binary(KeyName), B),
            maps:put(Key, Value, Acc)
        end, #{}, lib_types:to_list(maps:get(lib_types:to_binary(Name), Bson, [])));
get_bson_value(Type, _, Name, Bson) ->
    bson_to_record(maps:get(lib_types:to_binary(Name), Bson, #{}), Type).

key_to_bson(Tab, Key) ->
    Field = get_key_field(Tab),
    set_bson_value(Field#field{value = Key}, #{}).

get_key_field(Tab) ->
    #table{key = Key} = db_table:get_table(Tab),
    FieldMap = db_table:get_field_map(Tab),
    maps:get(Key, FieldMap).

%%%%%%%%%%%%%%%%%%%%%%
%% 创建数据表
create_tabs() ->
    HasTabs = mnesia:system_info(tables),
    Tabs = db_table:role_tabs() ++ db_table:sys_tabs(),
    CreateTabs = lists:subtract(Tabs, HasTabs),
    create_tabs(CreateTabs),
    mnesia:wait_for_tables(CreateTabs, infinity).
create_tabs([]) ->
    ok;
create_tabs([Tab | Tail]) ->
    mnesia:create_table(Tab, [{disc_only_copies, [node()]}]),
    create_tabs(Tail).

%% 查找返回自增id
do_find_count(Key) ->
    Count = case mnesia:read({?DB_COUNT, Key}) of
                [Count0 = #db_count{}] -> Count0;
                [] -> #db_count{key = Key}
            end,
    NewCount = Count#db_count{value = Count#db_count.value + 1},
    ok = mnesia:write(NewCount),
    NewCount#db_count.value.

%% 待保存数据key_map
get_save_data_key_map() ->
    case erlang:get(?SAVE_DATA_KEY_MAP) of
        undefined -> #{};
        SaveDataKeyMap -> SaveDataKeyMap
    end.

put_save_data_key_map(SaveDataKeyMap) ->
    erlang:put(?SAVE_DATA_KEY_MAP, SaveDataKeyMap).