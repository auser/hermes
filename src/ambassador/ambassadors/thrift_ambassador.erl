%%%-------------------------------------------------------------------
%%% File    : 
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Wed Aug 12 14:19:36 PDT 2009
%%%-------------------------------------------------------------------
-module (thrift_ambassador).
-include ("hermes.hrl").

-include ("poolparty_types.hrl").
-include ("commandInterface_thrift.hrl").
-include ("poolparty_constants.hrl").

-behaviour(gen_server).

%% API
-export([start_link/1]).

-export ([
          ask/3,
          stop/0
         ]).

-export ([start_thrift_server/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
          start_args,   % args to start with
          thrift_pid    % thrift client pid
       }).
                 
-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Args) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Args], []).

ask(CloudName, Func, Msg) ->
  gen_server:call(?MODULE, {cloud_query, CloudName, Func, Msg}).

%%--------------------------------------------------------------------
%% Function: stop () -> ok
%% Description: Stop ourselves and the socket_server
%%--------------------------------------------------------------------
stop() ->
  io:format("Stopping~n"),
  stop_thrift_client(config:read()),
  % thrift_socket_server:stop(get_hostname()),
  ok.
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
init([Args]) ->
  ThriftPort = proplists:get_value(proto_port, Args),
  stop_thrift_client(Args),
  case start_thrift_client(Args) of
    {error, Reason} ->
      io:format("Assuming the thrift_client is already started error: ~p~n", [Reason]),
      ok;
    Pid ->
      erlang:monitor(process, Pid),
      Pid
  end,
    
  {ok, HostName} = get_hostname(),
  BannerArr = [
    {"thrift_port", erlang:integer_to_list(ThriftPort)},
    {"thrift client hostname", HostName}
  ],
  loudmouth:banner("Started thrift", BannerArr),

  % O = thrift_client:start_link(HostName, ThriftPort, commandInterface_thrift),
  {ok, P} = thrift_client:start_link(HostName, ThriftPort, commandInterface_thrift),
  {ok, #state{
    start_args = Args,
    thrift_pid = P
  }}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({cloud_query, CloudName, Fun, Args}, _From, #state{thrift_pid = P} = State) ->
  Reply = cloud_query(P, CloudName, Fun, Args),
  {reply, Reply, State};
  
handle_call(_Request, _From, State) ->
  Reply = ok,
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
handle_info({'DOWN',Ref,process, _Pid, normal}, #state{start_args = Args} = State) -> 
  erlang:demonitor(Ref),
  case start_thrift_client(Args) of
    {error, Reason} ->
      io:format("Assuming the thrift_client is already started error: ~p~n", [Reason]),
      ok;
    P ->
      erlang:monitor(process, P),
      P
  end,
  {noreply, State};
  
handle_info(Info, State) ->
  ?INFO("Received info in ~p: ~p~n", [?MODULE, Info]),
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(Reason, #state{start_args = Args} = _State) ->
  io:format("Terminating: ~p~n", [Reason]),
  build_start_command("stop", Args),
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


%%====================================================================
% Start thrift server (in erlang) 
%%====================================================================

start_thrift_server(Args) ->
  ThriftPort  = proplists:get_value(proto_port, Args),
  Module      = proplists:get_value(module, Args),
  thrift_socket_server:start([
    {port, ThriftPort},
    {name, ambassador},
    {service, ambassador_thrift},
    {handler, Module},
    {socket_opts, [{recv_timeout, infinity}]}]),
  ok.

start_thrift_client(Args) ->  
  StartCmd = build_start_command("run", Args),
  ?INFO("Starting ~p: ~p~n", [?MODULE, StartCmd]),
  spawn_link(fun() -> os:cmd(StartCmd) end).
  
stop_thrift_client(Args) ->
  StopCmd = build_start_command("stop", Args),
  ?INFO("Stopping ~p: ~p~n", [?MODULE, StopCmd]),
  spawn_link(fun() -> os:cmd(StopCmd) end).

%%--------------------------------------------------------------------
%% Function: build_start_command (Action, Args) -> cloud thrift string
%% Description: build a start command
%%--------------------------------------------------------------------  
build_start_command(Action, Args) ->
  ThriftPort = proplists:get_value(proto_port, Args),
  PortArg = lists:append(["--port ", erlang:integer_to_list(ThriftPort)]),
  lists:append(["cloud thrift ", Action, " -- ", PortArg]).

%%====================================================================
%% Query on the thrift server
%%====================================================================

cloud_query(P, Name, Meth, Args) ->
  Query = #cloudQuery{name=Name},
  io:format("Query: ~p~n", [Query]),
  thrift_client:call(P, run_command, [Query, Meth, Args]).