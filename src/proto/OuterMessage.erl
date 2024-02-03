%% -*- coding: utf-8 -*-
%% 自动生成, 请勿编辑
-module('OuterMessage').

-include("OuterMessage.hrl").
-include("protobuffer.hrl").

-export([encode_msg/1, decode_msg/2]).

encode_msg(Msg)->
	SerialFieldList = convert_field_list(Msg),
	gpb_serialize:serialize_field_list(SerialFieldList).
decode_msg(DataBin, MsgNameAtom) ->
	FieldMap = fetch_field_map(MsgNameAtom),
	NewFieldMap = gpb_unserialize:unserialize_field_list(DataBin, FieldMap),
	field_map_to_msg(MsgNameAtom, NewFieldMap).

convert_field_list(#'H_A2C_LoginResponse'{}=Msg) ->
    #'H_A2C_LoginResponse'{'Address'=F1,'Token'=F2,'ErrorCode'=F91}=Msg,
    [
		#proto_field{field_tag=1,wire_type=?WIRE_TYPE_STRING_OR_MESSAGE,data_type=?DATA_TYPE_STRING,data_type_name='undefined',is_array=0,value=F1}, % Address
		#proto_field{field_tag=2,wire_type=?WIRE_TYPE_STRING_OR_MESSAGE,data_type=?DATA_TYPE_STRING,data_type_name='undefined',is_array=0,value=F2}, % Token
		#proto_field{field_tag=91,wire_type=?WIRE_TYPE_VARINT,data_type=?DATA_TYPE_UINT32,data_type_name='undefined',is_array=0,value=F91} % ErrorCode
    ];
convert_field_list(#'H_A2C_RegistrationResponse'{}=Msg) ->
    #'H_A2C_RegistrationResponse'{'ErrorCode'=F91}=Msg,
    [
		#proto_field{field_tag=91,wire_type=?WIRE_TYPE_VARINT,data_type=?DATA_TYPE_UINT32,data_type_name='undefined',is_array=0,value=F91} % ErrorCode
    ];
convert_field_list(#'H_C2A_LoginRequest'{}=Msg) ->
    #'H_C2A_LoginRequest'{'UserName'=F1,'Password'=F2}=Msg,
    [
		#proto_field{field_tag=1,wire_type=?WIRE_TYPE_STRING_OR_MESSAGE,data_type=?DATA_TYPE_STRING,data_type_name='undefined',is_array=0,value=F1}, % UserName
		#proto_field{field_tag=2,wire_type=?WIRE_TYPE_STRING_OR_MESSAGE,data_type=?DATA_TYPE_STRING,data_type_name='undefined',is_array=0,value=F2} % Password
    ];
convert_field_list(#'H_C2A_RegistrationRequest'{}=Msg) ->
    #'H_C2A_RegistrationRequest'{'UserName'=F1,'Password'=F2,'Email'=F3}=Msg,
    [
		#proto_field{field_tag=1,wire_type=?WIRE_TYPE_STRING_OR_MESSAGE,data_type=?DATA_TYPE_STRING,data_type_name='undefined',is_array=0,value=F1}, % UserName
		#proto_field{field_tag=2,wire_type=?WIRE_TYPE_STRING_OR_MESSAGE,data_type=?DATA_TYPE_STRING,data_type_name='undefined',is_array=0,value=F2}, % Password
		#proto_field{field_tag=3,wire_type=?WIRE_TYPE_STRING_OR_MESSAGE,data_type=?DATA_TYPE_STRING,data_type_name='undefined',is_array=0,value=F3} % Email
    ];
convert_field_list(#'H_C2G_LoginRequest'{}=Msg) ->
    #'H_C2G_LoginRequest'{'Token'=F1}=Msg,
    [
		#proto_field{field_tag=1,wire_type=?WIRE_TYPE_STRING_OR_MESSAGE,data_type=?DATA_TYPE_STRING,data_type_name='undefined',is_array=0,value=F1} % Token
    ];
convert_field_list(#'H_C2G_PlayGameRequest'{}=Msg) ->
    #'H_C2G_PlayGameRequest'{'Level'=F1}=Msg,
    [
		#proto_field{field_tag=1,wire_type=?WIRE_TYPE_VARINT,data_type=?DATA_TYPE_INT32,data_type_name='undefined',is_array=0,value=F1} % Level
    ];
convert_field_list(#'H_G2C_LoginResponse'{}=Msg) ->
    #'H_G2C_LoginResponse'{'RoleInfo'=F1,'ErrorCode'=F91}=Msg,
    [
		#proto_field{field_tag=1,wire_type=?WIRE_TYPE_STRING_OR_MESSAGE,data_type=?DATA_TYPE_MESSAGE,data_type_name='RoleInfo',is_array=0,value=F1}, % RoleInfo
		#proto_field{field_tag=91,wire_type=?WIRE_TYPE_VARINT,data_type=?DATA_TYPE_UINT32,data_type_name='undefined',is_array=0,value=F91} % ErrorCode
    ];
convert_field_list(#'H_G2C_PlayGameResponse'{}=Msg) ->
    #'H_G2C_PlayGameResponse'{'ErrorCode'=F91}=Msg,
    [
		#proto_field{field_tag=91,wire_type=?WIRE_TYPE_VARINT,data_type=?DATA_TYPE_UINT32,data_type_name='undefined',is_array=0,value=F91} % ErrorCode
    ];
convert_field_list(#'H_G2C_RepeatLogin'{}=Msg) ->
    #'H_G2C_RepeatLogin'{}=Msg,
    [
    ];
convert_field_list(#'ProtoLogin_ReqLogin'{}=Msg) ->
    #'ProtoLogin_ReqLogin'{'player_id'=F1}=Msg,
    [
		#proto_field{field_tag=1,wire_type=?WIRE_TYPE_VARINT,data_type=?DATA_TYPE_UINT32,data_type_name='undefined',is_array=0,value=F1} % player_id
    ];
convert_field_list(#'ProtoLogin_ReqTest'{}=Msg) ->
    #'ProtoLogin_ReqTest'{}=Msg,
    [
    ];
convert_field_list(#'ProtoLogin_ResLogin'{}=Msg) ->
    #'ProtoLogin_ResLogin'{'login_state'=F1,'ErrorCode'=F91}=Msg,
    [
		#proto_field{field_tag=1,wire_type=?WIRE_TYPE_VARINT,data_type=?DATA_TYPE_BOOL,data_type_name='undefined',is_array=0,value=F1}, % login_state
		#proto_field{field_tag=91,wire_type=?WIRE_TYPE_VARINT,data_type=?DATA_TYPE_UINT32,data_type_name='undefined',is_array=0,value=F91} % ErrorCode
    ];
convert_field_list(#'ProtoLogin_ResTest'{}=Msg) ->
    #'ProtoLogin_ResTest'{'online_players'=F1,'test1'=F2,'test2'=F3,'ErrorCode'=F91}=Msg,
    [
		#proto_field{field_tag=1,wire_type=?WIRE_TYPE_STRING_OR_MESSAGE,data_type=?DATA_TYPE_MESSAGE,data_type_name='ProtoOnline',is_array=1,value=F1}, % online_players
		#proto_field{field_tag=2,wire_type=?WIRE_TYPE_VARINT,data_type=?DATA_TYPE_INT32,data_type_name='undefined',is_array=0,value=F2}, % test1
		#proto_field{field_tag=3,wire_type=?WIRE_TYPE_VARINT,data_type=?DATA_TYPE_BOOL,data_type_name='undefined',is_array=0,value=F3}, % test2
		#proto_field{field_tag=91,wire_type=?WIRE_TYPE_VARINT,data_type=?DATA_TYPE_UINT32,data_type_name='undefined',is_array=0,value=F91} % ErrorCode
    ];
convert_field_list(#'ProtoOnline'{}=Msg) ->
    #'ProtoOnline'{'player_id'=F1,'is_online'=F2}=Msg,
    [
		#proto_field{field_tag=1,wire_type=?WIRE_TYPE_VARINT,data_type=?DATA_TYPE_INT64,data_type_name='undefined',is_array=0,value=F1}, % player_id
		#proto_field{field_tag=2,wire_type=?WIRE_TYPE_VARINT,data_type=?DATA_TYPE_BOOL,data_type_name='undefined',is_array=0,value=F2} % is_online
    ];
convert_field_list(#'RoleInfo'{}=Msg) ->
    #'RoleInfo'{'AccountId'=F1,'Level'=F2,'MaxLevel'=F3}=Msg,
    [
		#proto_field{field_tag=1,wire_type=?WIRE_TYPE_VARINT,data_type=?DATA_TYPE_INT64,data_type_name='undefined',is_array=0,value=F1}, % AccountId
		#proto_field{field_tag=2,wire_type=?WIRE_TYPE_VARINT,data_type=?DATA_TYPE_INT32,data_type_name='undefined',is_array=0,value=F2}, % Level
		#proto_field{field_tag=3,wire_type=?WIRE_TYPE_VARINT,data_type=?DATA_TYPE_INT32,data_type_name='undefined',is_array=0,value=F3} % MaxLevel
    ];
convert_field_list(_) ->
	[].

fetch_field_map('H_A2C_LoginResponse') ->
    #{
		1 => #proto_field{field_tag=1,wire_type=?WIRE_TYPE_STRING_OR_MESSAGE,data_type=?DATA_TYPE_STRING,data_type_name='undefined',is_array=0}, % Address
		2 => #proto_field{field_tag=2,wire_type=?WIRE_TYPE_STRING_OR_MESSAGE,data_type=?DATA_TYPE_STRING,data_type_name='undefined',is_array=0}, % Token
		91 => #proto_field{field_tag=91,wire_type=?WIRE_TYPE_VARINT,data_type=?DATA_TYPE_UINT32,data_type_name='undefined',is_array=0} % ErrorCode
    };
fetch_field_map('H_A2C_RegistrationResponse') ->
    #{
		91 => #proto_field{field_tag=91,wire_type=?WIRE_TYPE_VARINT,data_type=?DATA_TYPE_UINT32,data_type_name='undefined',is_array=0} % ErrorCode
    };
fetch_field_map('H_C2A_LoginRequest') ->
    #{
		1 => #proto_field{field_tag=1,wire_type=?WIRE_TYPE_STRING_OR_MESSAGE,data_type=?DATA_TYPE_STRING,data_type_name='undefined',is_array=0}, % UserName
		2 => #proto_field{field_tag=2,wire_type=?WIRE_TYPE_STRING_OR_MESSAGE,data_type=?DATA_TYPE_STRING,data_type_name='undefined',is_array=0} % Password
    };
fetch_field_map('H_C2A_RegistrationRequest') ->
    #{
		1 => #proto_field{field_tag=1,wire_type=?WIRE_TYPE_STRING_OR_MESSAGE,data_type=?DATA_TYPE_STRING,data_type_name='undefined',is_array=0}, % UserName
		2 => #proto_field{field_tag=2,wire_type=?WIRE_TYPE_STRING_OR_MESSAGE,data_type=?DATA_TYPE_STRING,data_type_name='undefined',is_array=0}, % Password
		3 => #proto_field{field_tag=3,wire_type=?WIRE_TYPE_STRING_OR_MESSAGE,data_type=?DATA_TYPE_STRING,data_type_name='undefined',is_array=0} % Email
    };
fetch_field_map('H_C2G_LoginRequest') ->
    #{
		1 => #proto_field{field_tag=1,wire_type=?WIRE_TYPE_STRING_OR_MESSAGE,data_type=?DATA_TYPE_STRING,data_type_name='undefined',is_array=0} % Token
    };
fetch_field_map('H_C2G_PlayGameRequest') ->
    #{
		1 => #proto_field{field_tag=1,wire_type=?WIRE_TYPE_VARINT,data_type=?DATA_TYPE_INT32,data_type_name='undefined',is_array=0} % Level
    };
fetch_field_map('H_G2C_LoginResponse') ->
    #{
		1 => #proto_field{field_tag=1,wire_type=?WIRE_TYPE_STRING_OR_MESSAGE,data_type=?DATA_TYPE_MESSAGE,data_type_name='RoleInfo',is_array=0}, % RoleInfo
		91 => #proto_field{field_tag=91,wire_type=?WIRE_TYPE_VARINT,data_type=?DATA_TYPE_UINT32,data_type_name='undefined',is_array=0} % ErrorCode
    };
fetch_field_map('H_G2C_PlayGameResponse') ->
    #{
		91 => #proto_field{field_tag=91,wire_type=?WIRE_TYPE_VARINT,data_type=?DATA_TYPE_UINT32,data_type_name='undefined',is_array=0} % ErrorCode
    };
fetch_field_map('H_G2C_RepeatLogin') ->
    #{
    };
fetch_field_map('ProtoLogin_ReqLogin') ->
    #{
		1 => #proto_field{field_tag=1,wire_type=?WIRE_TYPE_VARINT,data_type=?DATA_TYPE_UINT32,data_type_name='undefined',is_array=0} % player_id
    };
fetch_field_map('ProtoLogin_ReqTest') ->
    #{
    };
fetch_field_map('ProtoLogin_ResLogin') ->
    #{
		1 => #proto_field{field_tag=1,wire_type=?WIRE_TYPE_VARINT,data_type=?DATA_TYPE_BOOL,data_type_name='undefined',is_array=0}, % login_state
		91 => #proto_field{field_tag=91,wire_type=?WIRE_TYPE_VARINT,data_type=?DATA_TYPE_UINT32,data_type_name='undefined',is_array=0} % ErrorCode
    };
fetch_field_map('ProtoLogin_ResTest') ->
    #{
		1 => #proto_field{field_tag=1,wire_type=?WIRE_TYPE_STRING_OR_MESSAGE,data_type=?DATA_TYPE_MESSAGE,data_type_name='ProtoOnline',is_array=1}, % online_players
		2 => #proto_field{field_tag=2,wire_type=?WIRE_TYPE_VARINT,data_type=?DATA_TYPE_INT32,data_type_name='undefined',is_array=0}, % test1
		3 => #proto_field{field_tag=3,wire_type=?WIRE_TYPE_VARINT,data_type=?DATA_TYPE_BOOL,data_type_name='undefined',is_array=0}, % test2
		91 => #proto_field{field_tag=91,wire_type=?WIRE_TYPE_VARINT,data_type=?DATA_TYPE_UINT32,data_type_name='undefined',is_array=0} % ErrorCode
    };
fetch_field_map('ProtoOnline') ->
    #{
		1 => #proto_field{field_tag=1,wire_type=?WIRE_TYPE_VARINT,data_type=?DATA_TYPE_INT64,data_type_name='undefined',is_array=0}, % player_id
		2 => #proto_field{field_tag=2,wire_type=?WIRE_TYPE_VARINT,data_type=?DATA_TYPE_BOOL,data_type_name='undefined',is_array=0} % is_online
    };
fetch_field_map('RoleInfo') ->
    #{
		1 => #proto_field{field_tag=1,wire_type=?WIRE_TYPE_VARINT,data_type=?DATA_TYPE_INT64,data_type_name='undefined',is_array=0}, % AccountId
		2 => #proto_field{field_tag=2,wire_type=?WIRE_TYPE_VARINT,data_type=?DATA_TYPE_INT32,data_type_name='undefined',is_array=0}, % Level
		3 => #proto_field{field_tag=3,wire_type=?WIRE_TYPE_VARINT,data_type=?DATA_TYPE_INT32,data_type_name='undefined',is_array=0} % MaxLevel
    };
fetch_field_map(_) ->
	#{}.

field_map_to_msg('H_A2C_LoginResponse', _FieldMap) ->
    #'H_A2C_LoginResponse'{
		'Address' = gpb_unserialize:get_value(maps:get(1,_FieldMap)),
		'Token' = gpb_unserialize:get_value(maps:get(2,_FieldMap)),
		'ErrorCode' = gpb_unserialize:get_value(maps:get(91,_FieldMap))
    };
field_map_to_msg('H_A2C_RegistrationResponse', _FieldMap) ->
    #'H_A2C_RegistrationResponse'{
		'ErrorCode' = gpb_unserialize:get_value(maps:get(91,_FieldMap))
    };
field_map_to_msg('H_C2A_LoginRequest', _FieldMap) ->
    #'H_C2A_LoginRequest'{
		'UserName' = gpb_unserialize:get_value(maps:get(1,_FieldMap)),
		'Password' = gpb_unserialize:get_value(maps:get(2,_FieldMap))
    };
field_map_to_msg('H_C2A_RegistrationRequest', _FieldMap) ->
    #'H_C2A_RegistrationRequest'{
		'UserName' = gpb_unserialize:get_value(maps:get(1,_FieldMap)),
		'Password' = gpb_unserialize:get_value(maps:get(2,_FieldMap)),
		'Email' = gpb_unserialize:get_value(maps:get(3,_FieldMap))
    };
field_map_to_msg('H_C2G_LoginRequest', _FieldMap) ->
    #'H_C2G_LoginRequest'{
		'Token' = gpb_unserialize:get_value(maps:get(1,_FieldMap))
    };
field_map_to_msg('H_C2G_PlayGameRequest', _FieldMap) ->
    #'H_C2G_PlayGameRequest'{
		'Level' = gpb_unserialize:get_value(maps:get(1,_FieldMap))
    };
field_map_to_msg('H_G2C_LoginResponse', _FieldMap) ->
    #'H_G2C_LoginResponse'{
		'RoleInfo' = gpb_unserialize:get_value(maps:get(1,_FieldMap)),
		'ErrorCode' = gpb_unserialize:get_value(maps:get(91,_FieldMap))
    };
field_map_to_msg('H_G2C_PlayGameResponse', _FieldMap) ->
    #'H_G2C_PlayGameResponse'{
		'ErrorCode' = gpb_unserialize:get_value(maps:get(91,_FieldMap))
    };
field_map_to_msg('H_G2C_RepeatLogin', _FieldMap) ->
    #'H_G2C_RepeatLogin'{
    };
field_map_to_msg('ProtoLogin_ReqLogin', _FieldMap) ->
    #'ProtoLogin_ReqLogin'{
		'player_id' = gpb_unserialize:get_value(maps:get(1,_FieldMap))
    };
field_map_to_msg('ProtoLogin_ReqTest', _FieldMap) ->
    #'ProtoLogin_ReqTest'{
    };
field_map_to_msg('ProtoLogin_ResLogin', _FieldMap) ->
    #'ProtoLogin_ResLogin'{
		'login_state' = gpb_unserialize:get_value(maps:get(1,_FieldMap)),
		'ErrorCode' = gpb_unserialize:get_value(maps:get(91,_FieldMap))
    };
field_map_to_msg('ProtoLogin_ResTest', _FieldMap) ->
    #'ProtoLogin_ResTest'{
		'online_players' = gpb_unserialize:get_value(maps:get(1,_FieldMap)),
		'test1' = gpb_unserialize:get_value(maps:get(2,_FieldMap)),
		'test2' = gpb_unserialize:get_value(maps:get(3,_FieldMap)),
		'ErrorCode' = gpb_unserialize:get_value(maps:get(91,_FieldMap))
    };
field_map_to_msg('ProtoOnline', _FieldMap) ->
    #'ProtoOnline'{
		'player_id' = gpb_unserialize:get_value(maps:get(1,_FieldMap)),
		'is_online' = gpb_unserialize:get_value(maps:get(2,_FieldMap))
    };
field_map_to_msg('RoleInfo', _FieldMap) ->
    #'RoleInfo'{
		'AccountId' = gpb_unserialize:get_value(maps:get(1,_FieldMap)),
		'Level' = gpb_unserialize:get_value(maps:get(2,_FieldMap)),
		'MaxLevel' = gpb_unserialize:get_value(maps:get(3,_FieldMap))
    };
field_map_to_msg(_, _) ->
	undefined.

