%% -*- coding: utf-8 -*-

-module(lib_tcp).

-include("common.hrl").
-include("db_table.hrl").
-include("OuterMessage.hrl").

%% API
-export([
    server/0,
    pack_to_bin/3
]).

-record(unpack_state, {sid, player_id, wait_bin = <<>>}).

server() ->
    spawn(fun() -> listen() end).

listen() ->
    {ok, Listen} = gen_tcp:listen(config:tcp_port(), [binary, {packet, 0}, {active, true}, {reuseaddr, true}]),
    spawn(fun() -> connect_client(Listen) end),
    loop_listen().

loop_listen() ->
    gen_tcp_socket:listen(),
    receive
        _Msg -> 
	    ?INFO("~w", [_Msg]),
	    loop_listen()
    end.

connect_client(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    {ok, {Ip, Port}} = inet:peername(Socket),
    ?INFO("client connect, ip: ~w, port: ~w", [Ip, Port]),
    spawn(fun() -> connect_client(Listen) end),
    Sid = self(),
    Pid = spawn_link(fun() -> loop_unpack(#unpack_state{sid = Sid}) end),
    loop_server(Socket, Pid).

loop_server(Socket, Pid) ->
    receive
        {tcp, Socket, Bin} ->
            Pid ! {unpack, Bin},
            loop_server(Socket, Pid);
        {send_to_client, Bin} ->
            gen_tcp:send(Socket, Bin),
            loop_server(Socket, Pid);
        {tcp_closed, _Socket} ->
            ?INFO("tcp_closed: ~w, link: ~w", [self(), Pid]),
            Pid ! tcp_closed;
        disconnect ->
            gen_tcp:close(Socket)
    end.

loop_unpack(UnpackState) ->
    receive
        {unpack, Bin} ->
            NewUnpackState = unpack_bin(UnpackState, Bin),
            loop_unpack(NewUnpackState);
        tcp_closed ->
            lib_login:logout(UnpackState#unpack_state.player_id)
    end.

unpack_bin(#unpack_state{wait_bin = <<>>, player_id = PlayerId, sid = Sid} = UnpackState, PackBin) ->
    <<BodySize:32/little, Code:32/little, RpcId:32/little, OpCodeType:64/little, Bin/binary>> = PackBin,
    Size = byte_size(Bin),
    case BodySize =< Size of
        true ->
            NewBin = binary:part(Bin, 0, BodySize),
            MsgName = 'OuterMessage_code':get_msg_name(Code),
            Msg = 'OuterMessage':decode_msg(NewBin, MsgName),
            ?INFO("client msg: ~w", [Msg]),
            NewPlayerId = handle(Msg, PlayerId, Sid, RpcId, OpCodeType),
            NewSize = byte_size(NewBin),
            case NewSize >= Size of
                true -> UnpackState#unpack_state{player_id = NewPlayerId};
                false ->
                    WaitBin = binary:part(binary:list_to_bin(lists:reverse(binary:bin_to_list(Bin))), 0, BodySize),
                    NewWaitBin = binary:list_to_bin(lists:reverse(binary:bin_to_list(WaitBin))),
                    UnpackState#unpack_state{wait_bin = NewWaitBin, player_id = NewPlayerId}
            end;
        false ->
            UnpackState#unpack_state{wait_bin = PackBin}
    end;
unpack_bin(#unpack_state{wait_bin = WaitBin} = UnpackState, PackBin) ->
    NewPackBin = <<WaitBin/binary, PackBin/binary>>,
    case NewPackBin of
        <<_BodySize:32/little, _Code:32/little, _RpcId:32/little, _OpCodeType:16/little, _Bin/binary>> ->
            unpack_bin(UnpackState#unpack_state{wait_bin = <<>>}, NewPackBin);
        _ ->
            UnpackState#unpack_state{wait_bin = NewPackBin}
    end.

handle(#'ProtoLogin_ReqLogin'{player_id = PlayerId} = _Msg, OldPlayerId, Sid, RpcId, OpCodeType) ->
    case lib_db:load_one(?DB_ROLE, PlayerId) of
        undefined ->
            Bin = pack_to_bin(#'ProtoLogin_ResLogin'{login_state = false}, RpcId, OpCodeType),
            Sid ! {send_to_client, Bin},
            OldPlayerId;
        _DbRole ->
            lib_login:login(PlayerId, Sid),
            Bin = pack_to_bin(#'ProtoLogin_ResLogin'{login_state = true}, RpcId, OpCodeType),
            Sid ! {send_to_client, Bin},
            PlayerId
    end;
handle(#'ProtoLogin_ReqTest'{} = _Msg, OldPlayerId, Sid, RpcId, OpCodeType) ->
    OnlinePlayers = lists:foldl(fun(N, Acc) -> [#'ProtoOnline'{player_id = N, is_online = rand:uniform(2) - 1} | Acc] end, [], lists:seq(1,10000)),
    Bin = pack_to_bin(#'ProtoLogin_ResTest'{online_players = OnlinePlayers}, RpcId, OpCodeType),
    Sid ! {send_to_client, Bin},
    OldPlayerId;
handle(Msg, PlayerId, Sid, RpcId, OpCodeType) ->
    case role_server:get_pid(PlayerId) of
        undefined -> Sid ! disconnect;
        Pid -> Pid ! {socket, Msg, RpcId, OpCodeType}
    end,
    PlayerId.

pack_to_bin(Msg, RpcId, OpCodeType) ->
    MsgName = element(1, Msg),
    Code = 'OuterMessage_code':get_code(MsgName),
    BodyBin = 'OuterMessage':encode_msg(Msg),
    BodySize = byte_size(BodyBin),
    <<BodySize:32/little, Code:32/little, RpcId:32/little, OpCodeType:64/little, BodyBin/binary>>.
