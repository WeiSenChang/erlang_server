%% -*- coding: utf-8 -*-
%% 自动生成, 请勿编辑
-module('OuterMessage_code').

-export([get_msg_name/1, get_code/1]).

get_msg_name(110000001) -> 'H_C2A_LoginRequest';
get_msg_name(110000002) -> 'H_C2A_RegistrationRequest';
get_msg_name(160000001) -> 'H_A2C_RegistrationResponse';
get_msg_name(160000002) -> 'H_A2C_LoginResponse';
get_msg_name(110000003) -> 'H_C2G_LoginRequest';
get_msg_name(160000003) -> 'H_G2C_LoginResponse';
get_msg_name(100000001) -> 'H_G2C_RepeatLogin';
get_msg_name(110000004) -> 'H_C2G_PlayGameRequest';
get_msg_name(160000004) -> 'H_G2C_PlayGameResponse';
get_msg_name(110000005) -> 'ProtoLogin_ReqLogin';
get_msg_name(160000005) -> 'ProtoLogin_ResLogin';
get_msg_name(110000006) -> 'ProtoLogin_ReqTest';
get_msg_name(160000006) -> 'ProtoLogin_ResTest';
get_msg_name(_) -> undefined.

get_code('H_C2A_LoginRequest') -> 110000001;
get_code('H_C2A_RegistrationRequest') -> 110000002;
get_code('H_A2C_RegistrationResponse') -> 160000001;
get_code('H_A2C_LoginResponse') -> 160000002;
get_code('H_C2G_LoginRequest') -> 110000003;
get_code('H_G2C_LoginResponse') -> 160000003;
get_code('H_G2C_RepeatLogin') -> 100000001;
get_code('H_C2G_PlayGameRequest') -> 110000004;
get_code('H_G2C_PlayGameResponse') -> 160000004;
get_code('ProtoLogin_ReqLogin') -> 110000005;
get_code('ProtoLogin_ResLogin') -> 160000005;
get_code('ProtoLogin_ReqTest') -> 110000006;
get_code('ProtoLogin_ResTest') -> 160000006;
get_code(_) -> undefined.

