%%%-------------------------------------------------------------------
%%% File    : ambassador.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Mon Aug 10 11:57:05 PDT 2009
%%%-------------------------------------------------------------------

-module (ambassador).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export ([
          ask_thrift/1,
          handle_function/2,
          get/1
          ]).
-export ([stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
        }).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
ask_thrift(Msg) ->
  Pid = get_thrift_pid(),
  thrift_client:call(Pid, get, [Msg]).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
  thrift_socket_server:stop(get_hostname()),
  ok.


%%%%% THRIFT INTERFACE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_function(Function, Args) when is_atom(Function), is_tuple(Args) ->
    % ?infoFmt("handling thrift stuff in PID ~p~n", [self()]),
    case apply(?MODULE, Function, tuple_to_list(Args)) of
        ok -> {ok, "handled"};
        Reply -> {reply, Reply}
    end.

get(Key) ->
  io:format("Get ~p in ~p~n", [Key, ?MODULE]),
  {ok, <<"Nice">>}.
  


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
  ThriftPort = case config:get(thrift_port) of
    {error, _} -> 11223;
    {ok, V} -> V
  end,
  thrift_socket_server:start([
    {port, ThriftPort},
    {name, ambassador},
    {service, ambassador_thrift},
    {handler, ?MODULE},
    {socket_opts, [{recv_timeout, infinity}]}]),
    
  {ok, HostName} = get_hostname(),
  loudmouth:banner([
    "Started thrift",
    {"thrift_port", erlang:integer_to_list(ThriftPort)},
    {"thrift client hostname", HostName}
  ]),
  
  thrift_client:start_link("localhost", ThriftPort, ambassador_thrift),
  
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%-------------------------------------------------------------------- 
handle_call({call, Function, Args}, _From, State) ->
  Out = handle_function(Function, Args),
  {reply, Out, State};
  
handle_call(_Request, _From, State) ->
  Reply = {ok, "Handled"},
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: get_hostname () -> HostName
%% Description: Quick accessor to local node's hostname
%% TODO: Make a commandline-passable-option
%%--------------------------------------------------------------------
get_hostname() -> inet:gethostname().

%%--------------------------------------------------------------------
%% Function: get_thrift_pid () -> {ok, Pid}
%% Description: Get the thrift_client pid
%%--------------------------------------------------------------------
get_thrift_pid() -> erlang:whereis(ambassador).