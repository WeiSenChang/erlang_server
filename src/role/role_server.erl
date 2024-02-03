%% -*- coding: utf-8 -*-

-module(role_server).

-behaviour(gen_server).

-include("common.hrl").
-include("server.hrl").
-include("db_table.hrl").
-include("role.hrl").

%% API
-export([get_pid/1, get_p_name/1]).

%% gen_server callback
-export([start_link/2, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(HEARTBEAT_SEC, 3).

-record(role_server_state, {role_id = 0, sid = undefined}).

%%%===================================================================
%%% API
%%%===================================================================
get_pid(RoleId) ->
    erlang:whereis(get_p_name(RoleId)).

get_p_name(RoleId) ->
    lib_types:to_atom(lib_types:to_list(?MODULE) ++ "_" ++ lib_types:to_list(RoleId)).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(RoleId, Sid) ->
    gen_server:start_link({local, get_p_name(RoleId)}, ?MODULE, [RoleId, Sid], []).

init([RoleId, Sid]) ->
    erlang:process_flag(trap_exit, true),
    gen_server:cast(self(), db_init),
    {ok, #role_server_state{role_id = RoleId, sid = Sid}}.

handle_call(_Request, _From, State = #role_server_state{}) ->
    {reply, ok, State}.

handle_cast(db_init, State = #role_server_state{role_id = RoleId, sid = Sid}) ->
    %% do db_init
    db_init(RoleId),
    role_login(RoleId),
    lib_role_listen:listen_login(RoleId, Sid),
    {noreply, State};
handle_cast({role_change_name, Name}, State = #role_server_state{role_id = RoleId}) ->
    lib_role:role_change_name(RoleId, Name),
    {noreply, State};
handle_cast(_Request, State = #role_server_state{}) ->
    {noreply, State}.

handle_info({socket, Msg, RpcId, OpCodeType}, State = #role_server_state{role_id = RoleId, sid = Sid}) ->
    proto_msg_handle:handle(RoleId, Sid, Msg, RpcId, OpCodeType),
    {noreply, State};
handle_info(_Info, State = #role_server_state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #role_server_state{role_id = RoleId}) ->
    lib_role_listen:listen_logout(RoleId),
    lib_db:save(),
    ok.

code_change(_OldVsn, State = #role_server_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
db_init(RoleId) ->
    db_init(RoleId, db_table:role_tabs()).
db_init(_RoleId, []) ->
    ok;
db_init(RoleId, [Tab | Tail]) ->
    case lib_db:load_one(Tab, RoleId) of
        undefined -> ignore;
        Data -> lib_db:put_data(Tab, RoleId, Data)
    end,
    db_init(RoleId, Tail).

role_login(RoleId) ->
    lib_role:role_login(RoleId),
    lib_role_pack:role_login(RoleId),
    ok.