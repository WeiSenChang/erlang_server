%% -*- coding: utf-8 -*-

-module(role_manage_server).

-behaviour(gen_server).

-include("common.hrl").
-include("server.hrl").
-include("db_table.hrl").

%% API
-export([get_pid/0, get_p_name/0]).

%% gen_server callback
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(ON_OFF_LINE_TIME_OUT, 10000).

-record(role_manage_server_state, {}).

%%%===================================================================
%%% API
%%%===================================================================
get_pid() ->
    erlang:whereis(?MODULE).

get_p_name() ->
    ?MODULE.

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    erlang:process_flag(trap_exit, true),
    gen_server:cast(self(), db_init),
    lib_cache:set_server_state(?MODULE, ?SERVER_STARTING),
    erlang:send_after(?ON_OFF_LINE_TIME_OUT, self(), on_off_line),
    {ok, #role_manage_server_state{}}.

handle_call({create_role, Name}, _From, State = #role_manage_server_state{}) ->
    Reply = lib_role_manage:role_create([Name]),
    {reply, Reply, State};
handle_call({role_change_name, RoleId, OldName, Name}, _From, State = #role_manage_server_state{}) ->
    Reply = lib_role_manage:role_change_name([{RoleId, OldName, Name}]),
    {reply, Reply, State};
handle_call(_Request, _From, State = #role_manage_server_state{}) ->
    {reply, ok, State}.

handle_cast(db_init, State = #role_manage_server_state{}) ->
    %% do db_init
    db_init(),
    lib_cache:set_server_state(?MODULE, ?SERVER_STARTED),
    {noreply, State};
handle_cast({role_login, RoleId, Sid}, State = #role_manage_server_state{}) ->
    lib_role_manage:role_login(RoleId, Sid),
    {noreply, State};
handle_cast({role_logout, RoleId}, State = #role_manage_server_state{}) ->
    lib_role_manage:role_logout(RoleId),
    {noreply, State};
handle_cast(_Request, State = #role_manage_server_state{}) ->
    {noreply, State}.

handle_info(on_off_line, State = #role_manage_server_state{}) ->
    erlang:send_after(?ON_OFF_LINE_TIME_OUT, self(), on_off_line),
    lib_role_manage:update_on_off_line_map(),
    {noreply, State};
handle_info(_Info, State = #role_manage_server_state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #role_manage_server_state{}) ->
    lib_db:save(),
    ok.

code_change(_OldVsn, State = #role_manage_server_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
db_init() ->
    RoleList = lib_db:load_all(?DB_ROLE),
    {OffLineRoleMap, RoleNameMap} = get_offline_and_name_map(RoleList),
    lib_cache:set_offline_role_map(OffLineRoleMap),
    lib_cache:set_role_name_map(RoleNameMap),
    RoleShowList = lib_db:load_all(?DB_ROLE_SHOW),
    db_init(RoleShowList).
db_init([]) ->
    ok;
db_init([RoleShow | Tail]) ->
    lib_role_manage:put_data(RoleShow),
    db_init(Tail).

get_offline_and_name_map(Roles) ->
    get_offline_and_name_map(#{}, #{}, Roles).
get_offline_and_name_map(OffLineRoleMap, RoleNameMap, []) ->
    {OffLineRoleMap, RoleNameMap};
get_offline_and_name_map(OffLineRoleMap, RoleNameMap, [Role | Tail]) ->
    #db_role{role_id = RoleId, name = Name} = Role,
    NewOffLineRoleMap = maps:put(RoleId, 1, OffLineRoleMap),
    NewRoleNameMap = maps:put(Name, RoleId, RoleNameMap),
    get_offline_and_name_map(NewOffLineRoleMap, NewRoleNameMap, Tail).