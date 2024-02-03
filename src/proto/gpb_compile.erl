-module(gpb_compile).

-include("../../include/protobuffer.hrl").

-export([main/1]).

-define(MSG_TYPE_REC, 0).	%% 记录
-define(MSG_TYPE_REQ, 1).	%% 请求
-define(MSG_TYPE_RES, 2).	%% 返回
-define(MSG_TYPE_NTF, 3).	%% 通知

main(_) ->
	P = "./proto",
	I = "./include",
	O = "./src/proto",
	C = "./src/proto",
	filelib:ensure_dir(I),
	filelib:ensure_dir(O),
	filelib:ensure_dir(C),
	{ok, FileNames} = file:list_dir_all(P),
	compile(P, I, O, C, FileNames).

compile(_P, _I, _O, _C, []) ->
	ok;
compile(P, I, O, C, [FileName | T]) ->
	case string:tokens(FileName, ".") of
		[BaseName, "proto"] ->
			{FileMsgMap, FileMsgList} = compile_file(P, FileName),
			write_hrl_file(I, BaseName, FileMsgMap),
			write_erl_file(O, BaseName, FileMsgMap),
			write_code_file(C, BaseName ++ "_code", FileMsgList);
		_ ->
			ignore
	end,
	compile(P, I, O, C, T).

compile_file(P, FileName) ->
	FileRealPath = filename:join(P, FileName),
	io:format(FileRealPath ++ "~n"),
	{ok, FileHandler} = file:open(FileRealPath, [read]),
	{FileMsgMap, FileMsgList} = compile_file_to_map(#{}, [], FileHandler),
	file:close(FileHandler),
	{FileMsgMap, FileMsgList}.

compile_file_to_map(FileMsgMap, FileMsgList, FileHandler) ->
	case file:read_line(FileHandler) of
		eof ->
			{FileMsgMap, FileMsgList};
		{ok, [$m,$e,$s,$s,$a,$g,$e | NameTail]} ->
			{MsgName, IsAddErrCode, MsgType} =
				case string:tokens(NameTail, " ") of
					[MsgName0, "//" , [$I,$R,$e,$q,$u,$e,$s,$t|_] | _T] ->
						{MsgName0, false, ?MSG_TYPE_REQ};
					[MsgName0, "//" , [$I,$R,$e,$s,$p,$o,$n,$s,$e|_] | _T] ->
						{MsgName0, true, ?MSG_TYPE_RES};
					[MsgName0, "//" , [$I,$M,$e,$s,$s,$a,$g,$e|_] | _T] ->
						{MsgName0, false, ?MSG_TYPE_NTF};
					[MsgNames | _] ->
						[MsgName1 | _] = string:tokens(MsgNames, "\n"),
						{MsgName1, false, ?MSG_TYPE_REC}
				end,
			io:format("message: ~ts ~n", [MsgName]),
			NewFileMsgMap0 = maps:put(MsgName, [], FileMsgMap),
			NewFileMsgMap1 = compile_file_for_msgbody(NewFileMsgMap0, FileHandler, MsgName),
			TermList = maps:get(MsgName, NewFileMsgMap1),
			NewTermList =
				case IsAddErrCode of
					true ->
						Term = {"0", "uint32", "ErrorCode", "91"},
						[Term | TermList];
					false ->
						TermList
				end,
			NewFileMsgMap2 = maps:put(MsgName, NewTermList, NewFileMsgMap1),
			NewFileMsgList = [{MsgName, MsgType} | FileMsgList],
			compile_file_to_map(NewFileMsgMap2, NewFileMsgList, FileHandler);
		_ ->
			compile_file_to_map(FileMsgMap, FileMsgList, FileHandler)
	end.

compile_file_for_msgbody(FileMsgMap, FileHandler, MsgName) ->
	case file:read_line(FileHandler) of
		{ok, [$} | _T]} ->
			FileMsgMap;
		{ok, Line} ->
			case string:tokens(Line, "=") of
				[Line1, Line2] ->
					[Tags, _Comments] = string:tokens(Line2, ";"),
					[Tag] = string:tokens(Tags, " "),
					{IsArray, Type, FieldName} =
						case string:tokens(Line1, " ") of
							["repeated", Type0, FieldName0] -> {"1", Type0, FieldName0};
							[_, Type0, FieldName0] -> {"0", Type0, FieldName0};
							[Type0, FieldName0] -> {"0", Type0, FieldName0}
						end,
					Term = {IsArray, Type, FieldName, Tag},
					TermList = maps:get(MsgName, FileMsgMap),
					NewTermList = [Term | TermList],
					NewFileMsgMap = maps:put(MsgName, NewTermList, FileMsgMap),
					compile_file_for_msgbody(NewFileMsgMap, FileHandler, MsgName);
				_ ->
					compile_file_for_msgbody(FileMsgMap, FileHandler, MsgName)
			end
	end.

write_hrl_file(I, ModName, FileMsgMap) ->
	FileRealPath = filename:join(I, ModName) ++ ".hrl",
	io:format("write hrl to: ~ts~n", [FileRealPath]),
	{ok, FileHandle} = file:open(FileRealPath, [write, {encoding,utf8}]),
	io:format(FileHandle, "%% -*- coding: utf-8 -*-
%% 自动生成, 请勿编辑
-ifndef('~ts').
-define('~ts', true).~n", [ModName, ModName]),
	maps:fold(
		fun(MsgName, TermList, Acc0) ->
			io:format(FileHandle, "~n-record('~ts',{~n", [MsgName]),
			write_hrl_record_term(FileHandle, length(TermList), 1, lists:reverse(TermList)),
			io:format(FileHandle, "}).~n", []),
			Acc0
		end, ok, FileMsgMap),
	io:format(FileHandle, "~n-endif.~n", []),
	file:close(FileHandle).

write_erl_file(O, ModName, FileMsgMap) ->
	FileRealPath = filename:join(O, ModName) ++ ".erl",
	io:format("write erl to: ~ts~n", [FileRealPath]),
	{ok, FileHandle} = file:open(FileRealPath, [write, {encoding,utf8}]),
	io:format(FileHandle, "%% -*- coding: utf-8 -*-
%% 自动生成, 请勿编辑
-module('~ts').~n
-include(\"~ts.hrl\").
-include(\"protobuffer.hrl\").~n
-export([encode_msg/1, decode_msg/2]).~n
encode_msg(Msg)->
	SerialFieldList = convert_field_list(Msg),
	gpb_serialize:serialize_field_list(SerialFieldList).
decode_msg(DataBin, MsgNameAtom) ->
	FieldMap = fetch_field_map(MsgNameAtom),
	NewFieldMap = gpb_unserialize:unserialize_field_list(DataBin, FieldMap),
	field_map_to_msg(MsgNameAtom, NewFieldMap).~n~n", [ModName, ModName]),
	{ConvertString, FetchString, FieldToString} = maps:fold(
		fun(MsgName, TermList, {AccConvertString0, AccFetchString0, AccFieldToString0}) ->
			{ConvertString0, FetchString0, FieldToString0} = write_erl_by_terms(MsgName, lists:reverse(TermList)),
			{AccConvertString0 ++ ConvertString0, AccFetchString0 ++ FetchString0, AccFieldToString0 ++ FieldToString0}
		end, {"", "", ""}, FileMsgMap),
	file:write(FileHandle, ConvertString ++ "convert_field_list(_) ->\r\n\t[].\r\n\r\n"),
	file:write(FileHandle, FetchString ++ "fetch_field_map(_) ->\r\n\t#{}.\r\n\r\n"),
	file:write(FileHandle, FieldToString ++ "field_map_to_msg(_, _) ->\r\n\tundefined.\r\n\r\n"),
	file:close(FileHandle).

write_hrl_record_term(_FileHandle, MaxLen, Cur, _TermList) when Cur > MaxLen ->
	ok;
write_hrl_record_term(FileHandle, MaxLen, Cur, [{IsArray, Type, FieldName, Tag} | _T]) when Cur =:= MaxLen ->
	case IsArray of
		"1" ->
			file:write(FileHandle, array_field_to_hrl_string(Type, FieldName, Tag, ""));
		_ ->
			file:write(FileHandle, option_field_to_hrl_string(Type, FieldName, Tag, ""))
	end,
	ok;
write_hrl_record_term(FileHandle, MaxLen, Cur, [{IsArray, Type, FieldName, Tag} | T]) ->
	case IsArray of
		"1" ->
			file:write(FileHandle, array_field_to_hrl_string(Type, FieldName, Tag, ","));
		_ ->
			file:write(FileHandle, option_field_to_hrl_string(Type, FieldName, Tag, ","))
	end,
	write_hrl_record_term(FileHandle, MaxLen, Cur + 1, T).


write_code_file(C, ModName, FileMsgList) ->
	FileRealPath = filename:join(C, ModName) ++ ".erl",
	io:format("write erl to: ~ts~n", [FileRealPath]),
	{ok, FileHandle} = file:open(FileRealPath, [write, {encoding,utf8}]),
	io:format(FileHandle, "%% -*- coding: utf-8 -*-
%% 自动生成, 请勿编辑
-module('~ts').\r\n
-export([get_msg_name/1, get_code/1]).\r\n\r\n", [ModName]),
	{GetMsgString, GetCodeString, _, _, _} = lists:foldr(
		fun({MsgName, MsgType}, {AccGetMsgString, AccGetCodeString, AccReqCode, AccResCode, AccNtfCode}) ->
			write_code_by_terms(MsgType, MsgName, AccGetMsgString, AccGetCodeString, AccReqCode, AccResCode, AccNtfCode)
		end, {"", "", 110000001, 160000001, 100000001}, FileMsgList),
	file:write(FileHandle, GetMsgString ++ "get_msg_name(_) -> undefined.\r\n\r\n"),
	file:write(FileHandle, GetCodeString ++ "get_code(_) -> undefined.\r\n\r\n"),
	file:close(FileHandle).

write_code_by_terms(?MSG_TYPE_REQ, MsgName, GetMsgString, GetCodeString, ReqCode, ResCode, NtfCode) ->
	NewGetMsgString = GetMsgString ++ "get_msg_name(" ++ integer_to_list(ReqCode) ++ ") -> '" ++ MsgName ++ "';\r\n",
	NewGetCodeString = GetCodeString ++ "get_code('" ++ MsgName ++ "') -> " ++ integer_to_list(ReqCode) ++ ";\r\n",
	{NewGetMsgString, NewGetCodeString, ReqCode + 1, ResCode, NtfCode};
write_code_by_terms(?MSG_TYPE_RES, MsgName, GetMsgString, GetCodeString, ReqCode, ResCode, NtfCode) ->
	NewGetMsgString = GetMsgString ++ "get_msg_name(" ++ integer_to_list(ResCode) ++ ") -> '" ++ MsgName ++ "';\r\n",
	NewGetCodeString = GetCodeString ++ "get_code('" ++ MsgName ++ "') -> " ++ integer_to_list(ResCode) ++ ";\r\n",
	{NewGetMsgString, NewGetCodeString, ReqCode, ResCode + 1, NtfCode};
write_code_by_terms(?MSG_TYPE_NTF, MsgName, GetMsgString, GetCodeString, ReqCode, ResCode, NtfCode) ->
	NewGetMsgString = GetMsgString ++ "get_msg_name(" ++ integer_to_list(NtfCode) ++ ") -> '" ++ MsgName ++ "';\r\n",
	NewGetCodeString = GetCodeString ++ "get_code('" ++ MsgName ++ "') -> " ++ integer_to_list(NtfCode) ++ ";\r\n",
	{NewGetMsgString, NewGetCodeString, ReqCode, ResCode, NtfCode + 1};
write_code_by_terms(_, _MsgName, GetMsgString, GetCodeString, ReqCode, ResCode, NtfCode) ->
	{GetMsgString, GetCodeString, ReqCode, ResCode, NtfCode}.

%%%%%%%%%%
option_field_to_hrl_string("int32", FieldName, Tag, SplitChar) ->
	"    '" ++ FieldName ++ "'\t:: integer() | undefined" ++ SplitChar ++ " % = " ++ Tag ++ ", 32 bits\n";
option_field_to_hrl_string("int64", FieldName, Tag, SplitChar) ->
	"    '" ++ FieldName ++ "'\t:: integer() | undefined" ++ SplitChar ++ " % = " ++ Tag ++ ", 64 bits\n";
option_field_to_hrl_string("uint32", FieldName, Tag, SplitChar) ->
	"    '" ++ FieldName ++ "'\t:: integer() | undefined" ++ SplitChar ++ " % = " ++ Tag ++ ", 32 bits\n";
option_field_to_hrl_string("uint64", FieldName, Tag, SplitChar) ->
	"    '" ++ FieldName ++ "'\t:: integer() | undefined" ++ SplitChar ++ " % = " ++ Tag ++ ", 64 bits\n";
option_field_to_hrl_string("sint32", FieldName, Tag, SplitChar) ->
	"    '" ++ FieldName ++ "'\t:: integer() | undefined" ++ SplitChar ++ " % = " ++ Tag ++ ", 32 bits\n";
option_field_to_hrl_string("sint64", FieldName, Tag, SplitChar) ->
	"    '" ++ FieldName ++ "'\t:: integer() | undefined" ++ SplitChar ++ " % = " ++ Tag ++ ", 64 bits\n";
option_field_to_hrl_string("fixed32", FieldName, Tag, SplitChar) ->
	"    '" ++ FieldName ++ "'\t:: integer() | undefined" ++ SplitChar ++ " % = " ++ Tag ++ ", 32 bits\n";
option_field_to_hrl_string("fixed64", FieldName, Tag, SplitChar) ->
	"    '" ++ FieldName ++ "'\t:: integer() | undefined" ++ SplitChar ++ " % = " ++ Tag ++ ", 64 bits\n";
option_field_to_hrl_string("sfixed32", FieldName, Tag, SplitChar) ->
	"    '" ++ FieldName ++ "'\t:: integer() | undefined" ++ SplitChar ++ " % = " ++ Tag ++ ", 32 bits\n";
option_field_to_hrl_string("sfixed64", FieldName, Tag, SplitChar) ->
	"    '" ++ FieldName ++ "'\t:: integer() | undefined" ++ SplitChar ++ " % = " ++ Tag ++ ", 64 bits\n";
option_field_to_hrl_string("double", FieldName, Tag, SplitChar) ->
	"    '" ++ FieldName ++ "'\t:: float() | integer() | infinity | '-infinity' | nan | undefined" ++ SplitChar ++ " % = " ++ Tag ++ "\n";
option_field_to_hrl_string("float", FieldName, Tag, SplitChar) ->
	"    '" ++ FieldName ++ "'\t:: float() | integer() | infinity | '-infinity' | nan | undefined" ++ SplitChar ++ " % = " ++ Tag ++ "\n";
option_field_to_hrl_string("string", FieldName, Tag, SplitChar) ->
	"    '" ++ FieldName ++ "'\t:: iolist() | undefined" ++ SplitChar ++ " % = " ++ Tag ++ "\n";
option_field_to_hrl_string("bool", FieldName, Tag, SplitChar) ->
	"    '" ++ FieldName ++ "'\t:: boolean() | 0 | 1 | undefined" ++ SplitChar ++ " % = " ++ Tag ++ "\n";
option_field_to_hrl_string("bytes", FieldName, Tag, SplitChar) ->
	"    '" ++ FieldName ++ "'\t:: iodata() | undefined" ++ SplitChar ++ " % = " ++ Tag ++ "\n";
option_field_to_hrl_string(Type, FieldName, Tag, SplitChar) ->
	%% message子消息体
	"    '" ++ FieldName ++ "' = undefined" ++ SplitChar ++ " % = " ++ Tag ++ " #'" ++ Type ++ "'{}\n".

array_field_to_hrl_string("int32", FieldName, Tag, SplitChar) ->
	"    '" ++ FieldName ++ "'\t:: [integer()] | undefined" ++ SplitChar ++ " % = " ++ Tag ++ ", 32 bits\n";
array_field_to_hrl_string("int64", FieldName, Tag, SplitChar) ->
	"    '" ++ FieldName ++ "'\t:: [integer()] | undefined" ++ SplitChar ++ " % = " ++ Tag ++ ", 64 bits\n";
array_field_to_hrl_string("uint32", FieldName, Tag, SplitChar) ->
	"    '" ++ FieldName ++ "'\t:: [integer()] | undefined" ++ SplitChar ++ " % = " ++ Tag ++ ", 32 bits\n";
array_field_to_hrl_string("uint64", FieldName, Tag, SplitChar) ->
	"    '" ++ FieldName ++ "'\t:: [integer()] | undefined" ++ SplitChar ++ " % = " ++ Tag ++ ", 64 bits\n";
array_field_to_hrl_string("sint32", FieldName, Tag, SplitChar) ->
	"    '" ++ FieldName ++ "'\t:: [integer()] | undefined" ++ SplitChar ++ " % = " ++ Tag ++ ", 32 bits\n";
array_field_to_hrl_string("sint64", FieldName, Tag, SplitChar) ->
	"    '" ++ FieldName ++ "'\t:: [integer()] | undefined" ++ SplitChar ++ " % = " ++ Tag ++ ", 64 bits\n";
array_field_to_hrl_string("fixed32", FieldName, Tag, SplitChar) ->
	"    '" ++ FieldName ++ "'\t:: [integer()] | undefined" ++ SplitChar ++ " % = " ++ Tag ++ ", 32 bits\n";
array_field_to_hrl_string("fixed64", FieldName, Tag, SplitChar) ->
	"    '" ++ FieldName ++ "'\t:: [integer()] | undefined" ++ SplitChar ++ " % = " ++ Tag ++ ", 64 bits\n";
array_field_to_hrl_string("sfixed32", FieldName, Tag, SplitChar) ->
	"    '" ++ FieldName ++ "'\t:: [integer()] | undefined" ++ SplitChar ++ " % = " ++ Tag ++ ", 32 bits\n";
array_field_to_hrl_string("sfixed64", FieldName, Tag, SplitChar) ->
	"    '" ++ FieldName ++ "'\t:: [integer()] | undefined" ++ SplitChar ++ " % = " ++ Tag ++ ", 64 bits\n";
array_field_to_hrl_string("double", FieldName, Tag, SplitChar) ->
	"    '" ++ FieldName ++ "'\t:: [float()] | integer() | infinity | '-infinity' | nan | undefined" ++ SplitChar ++ " % = " ++ Tag ++ "\n";
array_field_to_hrl_string("float", FieldName, Tag, SplitChar) ->
	"    '" ++ FieldName ++ "'\t:: [float()] | integer() | infinity | '-infinity' | nan | undefined" ++ SplitChar ++ " % = " ++ Tag ++ "\n";
array_field_to_hrl_string("string", FieldName, Tag, SplitChar) ->
	"    '" ++ FieldName ++ "'\t:: [iolist()] | undefined" ++ SplitChar ++ " % = " ++ Tag ++ "\n";
array_field_to_hrl_string("bool", FieldName, Tag, SplitChar) ->
	"    '" ++ FieldName ++ "'\t:: [boolean()] | 0 | 1 | undefined" ++ SplitChar ++ " % = " ++ Tag ++ "\n";
array_field_to_hrl_string("bytes", FieldName, Tag, SplitChar) ->
	"    '" ++ FieldName ++ "'\t:: [iodata()] | undefined" ++ SplitChar ++ " % = " ++ Tag ++ "\n";
array_field_to_hrl_string(Type, FieldName, Tag, SplitChar) ->
	%% message子消息体
	"    '" ++ FieldName ++ "' = []" ++ SplitChar ++ " % = " ++ Tag ++ " [#'" ++ Type ++ "'{}]\n".


write_erl_by_terms(MsgName, TermList) ->
	ConverHeadString = "convert_field_list(#'" ++ MsgName ++ "'{}=Msg) ->\n",
	MsgReadString = "    #'" ++ MsgName ++ "'{",
	ConvertBodyString = "    [\n",
	FetchString = "fetch_field_map('" ++ MsgName ++ "') ->
    #{\n",
	FieldToString = "field_map_to_msg('" ++ MsgName ++ "', _FieldMap) ->
    #'" ++ MsgName ++ "'{\n",
	{NewMsgReadString, NewConvertBodyString, NewFetchString, NewFieldToString} =
		convert_erl_string_by_term_list(MsgReadString, ConvertBodyString, FetchString, FieldToString, TermList),
	{ConverHeadString ++ NewMsgReadString ++ NewConvertBodyString, NewFetchString, NewFieldToString}.

convert_erl_string_by_term_list(MsgReadString, ConvertBodyString, FetchString, FieldToString, []) ->
	{
			MsgReadString ++ "}=Msg,\n",
			ConvertBodyString ++ "    ];\n",
			FetchString ++ "    };\n",
			FieldToString ++ "    };\n"
	};
convert_erl_string_by_term_list(MsgReadString, ConvertBodyString, FetchString, FieldToString, [Term | T]) ->
	{IsArray, Type, FieldName, Tag} = Term,
	{WireType, DataType, DataTypeName} = get_wire_and_data_type(Type),
	Str = case length(T) =< 0 of true -> ""; _ -> "," end,
	NewMsgReadString = MsgReadString ++ "'" ++ FieldName ++ "'=F" ++ Tag ++ Str,
	NewConvertBodyString = ConvertBodyString ++ "\t\t#proto_field{field_tag=" ++ Tag ++ ",wire_type=" ++ WireType ++
		",data_type=" ++ DataType ++ ",data_type_name='" ++ DataTypeName ++ "',is_array=" ++ IsArray ++ ",value=F"++ Tag ++ "}" ++ Str ++ " % " ++ FieldName ++ "\n",
	NewFetchString = FetchString ++ "\t\t" ++ Tag ++ " => #proto_field{field_tag="++ Tag ++ ",wire_type=" ++ WireType ++
		",data_type=" ++ DataType ++ ",data_type_name='" ++ DataTypeName ++ "',is_array=" ++ IsArray ++ "}" ++ Str ++ " % " ++ FieldName ++ "\n",
	NewFieldToString = FieldToString ++ "\t\t'" ++ FieldName ++ "' = gpb_unserialize:get_value(maps:get(" ++ Tag ++ ",_FieldMap))" ++ Str ++ "\n",
	convert_erl_string_by_term_list(NewMsgReadString, NewConvertBodyString, NewFetchString, NewFieldToString, T).

get_wire_and_data_type("int32") ->
	{"?WIRE_TYPE_VARINT", "?DATA_TYPE_INT32", "undefined"};
get_wire_and_data_type("int64") ->
	{"?WIRE_TYPE_VARINT", "?DATA_TYPE_INT64", "undefined"};
get_wire_and_data_type("uint32") ->
	{"?WIRE_TYPE_VARINT", "?DATA_TYPE_UINT32", "undefined"};
get_wire_and_data_type("uint64") ->
	{"?WIRE_TYPE_VARINT", "?DATA_TYPE_UINT32", "undefined"};
get_wire_and_data_type("sint32") ->
	{"?WIRE_TYPE_VARINT", "?DATA_TYPE_SINT32", "undefined"};
get_wire_and_data_type("sint64") ->
	{"?WIRE_TYPE_VARINT", "?DATA_TYPE_SINT64", "undefined"};
get_wire_and_data_type("fixed32") ->
	{"?WIRE_TYPE_FLOAT", "?DATA_TYPE_FIXED32", "undefined"};
get_wire_and_data_type("sfixed32") ->
	{"?WIRE_TYPE_FLOAT", "?DATA_TYPE_SFIXED32", "undefined"};
get_wire_and_data_type("fixed64") ->
	{"?WIRE_TYPE_DOUBLE", "?DATA_TYPE_FIXED64", "undefined"};
get_wire_and_data_type("sfixed64") ->
	{"?WIRE_TYPE_DOUBLE", "?DATA_TYPE_SFIXED64", "undefined"};
get_wire_and_data_type("double") ->
	{"?WIRE_TYPE_DOUBLE", "?DATA_TYPE_DOUBLE", "undefined"};
get_wire_and_data_type("float") ->
	{"?WIRE_TYPE_FLOAT", "?DATA_TYPE_FLOAT", "undefined"};
get_wire_and_data_type("string") ->
	{"?WIRE_TYPE_STRING_OR_MESSAGE", "?DATA_TYPE_STRING", "undefined"};
get_wire_and_data_type("bool") ->
	{"?WIRE_TYPE_VARINT", "?DATA_TYPE_BOOL", "undefined"};
get_wire_and_data_type("bytes") ->
	{"?WIRE_TYPE_STRING_OR_MESSAGE", "?DATA_TYPE_BYTES", "undefined"};
get_wire_and_data_type(Type) ->
	{"?WIRE_TYPE_STRING_OR_MESSAGE", "?DATA_TYPE_MESSAGE", Type}.