%%%-------------------------------------------------------------------
%%% File    : mon_server.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Wed Jul 29 19:28:46 PDT 2009
%%%-------------------------------------------------------------------

-module (mon_server).
-include ("hermes.hrl").
-behaviour(gen_server).

%% API
-export([start_link/1]).
-export ([add_monitor/1, list_monitors/0]).
-export ([get_average/1, get_average_over/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
        monitors = []
        }).
-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Monitors) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Monitors], []).

%%--------------------------------------------------------------------
%% Function: add_monitor (module) -> {ok, Pid}
%% Description: Add a monitor module to the list of monitors
%%--------------------------------------------------------------------
add_monitor(Module) -> gen_server:call(?MODULE, {add_monitor, Module}).

%%--------------------------------------------------------------------
%% Function: get_average (Module) -> {ok, Fetched}
%% Description: Get the average of a specific module
%%--------------------------------------------------------------------
get_average(Module) -> gen_server:call(?MODULE, {get_average, Module}).

%%--------------------------------------------------------------------
%% Function: get_average_over (Module, Seconds) -> {ok, Fetched}
%% Description: Get the average of a specific module over Seconds
%%--------------------------------------------------------------------
get_average_over(Module, Seconds) -> gen_server:call(?MODULE, {get_average, Module, Seconds}).

list_monitors() -> gen_server:call(?MODULE, {list_monitors}).
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
init(Monitors) ->
  [Mon] = Monitors,
  State = #state{
    monitors = Mon
   },
  {ok, State}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({list_monitors}, _From, #state{monitors = Monitors} = State) ->
  ?LOG_MESSAGE(io_lib:fwrite("Monitors: ~p~n", [Monitors])),
  {reply, [Monitors], State};

handle_call({add_monitor, Module}, _From, #state{monitors = Monitors} = _State) ->
  NewState = #state{monitors = lists:append([Monitors, [Module]])},
  {reply, ok, NewState};

handle_call({get_average, Module, Seconds}, _From, State) ->
  Fetched = handle_get_average(Module, Seconds),
  {reply, Fetched, State};
handle_call({get_average, Module}, _From, State) ->
  Fetched = handle_get_average(Module, 60),
  {reply, Fetched, State};
  
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
%% Function: handle_get_average (Module, Seconds) -> {ok, Averages}
%% Description: Handle the fetching of averages for rrd
%%              over the last Seconds
%%--------------------------------------------------------------------
handle_get_average(Module, Last) ->
  {Mega, Secs, _} = now(),
  Start = Mega*1000000 + Secs - Last,
  {ok, Fetched} = erlrrd:fetch(io_lib:fwrite("~p/~p.rrd AVERAGE --start ~p --end ~p", [?RRD_DIRECTORY, Module, Start, Start])),
  io:format("Fetched: ~p~n", [Fetched]),
  Fetched.