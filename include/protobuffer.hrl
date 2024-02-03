%%--- coding:utf-8 ---

-ifndef('protobuffer_HRL').
-define('protobuffer_HRL', true).

-define(WIRE_TYPE_VARINT, 0).	%% 可变长度整数,如: int32, int64, uint32, uint64, sint32, sint64, bool, enum
-define(WIRE_TYPE_DOUBLE, 1).	%% 固定64位,如: fixed64, sfixed64, double
-define(WIRE_TYPE_STRING_OR_MESSAGE, 2).	%% 字符串或内嵌消息体的类型, 如: string, bytes, message
-define(WIRE_TYPE_FLOAT, 5).	%% 固定32位,如:fixed32, sfixed32, float

-define(DATA_TYPE_INT32, 1).
-define(DATA_TYPE_INT64, 2).
-define(DATA_TYPE_UINT32, 3).
-define(DATA_TYPE_UINT64, 4).
-define(DATA_TYPE_DOUBLE, 5).
-define(DATA_TYPE_FLOAT, 6).
-define(DATA_TYPE_STRING, 7).
-define(DATA_TYPE_BOOL, 8).
-define(DATA_TYPE_BYTES, 9).
-define(DATA_TYPE_SINT32, 10).
-define(DATA_TYPE_SINT64, 11).
-define(DATA_TYPE_FIXED32, 12).
-define(DATA_TYPE_FIXED64, 13).
-define(DATA_TYPE_SFIXED32, 14).
-define(DATA_TYPE_SFIXED64, 15).
-define(DATA_TYPE_MESSAGE, 16).

-record(proto_field, {		%% 用于存储序列化时的必要信息
	field_tag = 0,		%% 字段顺序编号
	wire_type = 0,		%% wire_type类型
	data_type = 0,		%% 数据类型
	is_array = 0,		%% 0 单个元素, 1 数组
	data_type_name = undefined,	%% message类型的record名
	value = undefined	%% 字段值
}).

-endif.
