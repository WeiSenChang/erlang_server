%% -*- coding: utf-8 -*-
%% 自动生成, 请勿编辑
-ifndef('OuterMessage').
-define('OuterMessage', true).

-record('H_A2C_LoginResponse',{
    'Address'	:: iolist() | undefined, % = 1
    'Token'	:: iolist() | undefined, % = 2
    'ErrorCode'	:: integer() | undefined % = 91, 32 bits
}).

-record('H_A2C_RegistrationResponse',{
    'ErrorCode'	:: integer() | undefined % = 91, 32 bits
}).

-record('H_C2A_LoginRequest',{
    'UserName'	:: iolist() | undefined, % = 1
    'Password'	:: iolist() | undefined % = 2
}).

-record('H_C2A_RegistrationRequest',{
    'UserName'	:: iolist() | undefined, % = 1
    'Password'	:: iolist() | undefined, % = 2
    'Email'	:: iolist() | undefined % = 3
}).

-record('H_C2G_LoginRequest',{
    'Token'	:: iolist() | undefined % = 1
}).

-record('H_C2G_PlayGameRequest',{
    'Level'	:: integer() | undefined % = 1, 32 bits
}).

-record('H_G2C_LoginResponse',{
    'RoleInfo' = undefined, % = 1 #'RoleInfo'{}
    'ErrorCode'	:: integer() | undefined % = 91, 32 bits
}).

-record('H_G2C_PlayGameResponse',{
    'ErrorCode'	:: integer() | undefined % = 91, 32 bits
}).

-record('H_G2C_RepeatLogin',{
}).

-record('ProtoLogin_ReqLogin',{
    'player_id'	:: integer() | undefined % = 1, 64 bits
}).

-record('ProtoLogin_ReqTest',{
}).

-record('ProtoLogin_ResLogin',{
    'login_state'	:: boolean() | 0 | 1 | undefined, % = 1
    'ErrorCode'	:: integer() | undefined % = 91, 32 bits
}).

-record('ProtoLogin_ResTest',{
    'online_players' = [], % = 1 [#'ProtoOnline'{}]
    'test1'	:: integer() | undefined, % = 2, 32 bits
    'test2'	:: boolean() | 0 | 1 | undefined, % = 3
    'ErrorCode'	:: integer() | undefined % = 91, 32 bits
}).

-record('ProtoOnline',{
    'player_id'	:: integer() | undefined, % = 1, 64 bits
    'is_online'	:: boolean() | 0 | 1 | undefined % = 2
}).

-record('RoleInfo',{
    'AccountId'	:: integer() | undefined, % = 1, 64 bits
    'Level'	:: integer() | undefined, % = 2, 32 bits
    'MaxLevel'	:: integer() | undefined % = 3, 32 bits
}).

-endif.
