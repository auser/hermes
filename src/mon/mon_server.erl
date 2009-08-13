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
-export ([list_monitors/0]).
-export ([get_average/1, get_average_over/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {}).
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

%%--------------------------------------------------------------------
%% Function: list_monitors () -> {ok, List}
%% Description: Get the list of monitors and reasonable statistics
%%--------------------------------------------------------------------
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
init(_Args) ->
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
handle_call({list_monitors}, _From, State) ->
  Mons = get_monitors(),
  ?LOG_MESSAGE(io_lib:fwrite("Monitors: ~p~n", [Mons])),
  {reply, Mons, State};

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
  KnownMonitors = get_monitors(),
  
  case lists:member(Module, KnownMonitors) of
    false -> {unknown_monitor, []};
    true ->
      {Mega, Secs, _} = now(),
      StartTime = Mega*1000000 + Secs - Last,

      % Method
      M = lists:append([
        ?RRD_DIRECTORY, "/", erlang:atom_to_list(Module), ".", "rrd", % File Module.rrd
        " AVERAGE ",
        " --start ", erlang:integer_to_list(StartTime),
        " --end ", erlang:integer_to_list(StartTime + Last)
      ]),
      
      {ok, Fetched} = erlrrd:fetch(M),
      parse_rrd_return(Fetched)
  end.

%%--------------------------------------------------------------------
%% Function: parse_rrd_return (Arr) -> {ok, Parsed}
%% Description: Take the output from erlrrd and turn it into something usable
% [["                            cpu"],
%  [[]],
%  ["1249284300: 5.0000000000e-01"]]
%%--------------------------------------------------------------------
parse_rrd_return(Arr) ->
  parse_rrd_return_1(Arr).

parse_rrd_return_1([[Desc]|Rest]) ->
  Module = erlang:list_to_atom(string:strip(Desc)),
  [_|ArrOfValues] = Rest,
  Values = lists:map(fun([Line]) -> collect_rrd_values(Line) end, ArrOfValues),
  {Module, Values}.
 
collect_rrd_values([]) -> {};
collect_rrd_values(Str) ->
  [Time|[V]] = string:tokens(Str, ":"),
  Val = case string:strip(V) of
    "nan" -> 0.0;
    F -> erlang:list_to_float(F)
  end,
  {Time, Val}.

%%--------------------------------------------------------------------
%% Function: get_monitors (Args) -> {ok, Monitors}
%% Description: Get the monitors either in the known directory or at the
%% behest of the user on the command-line
%%--------------------------------------------------------------------
get_monitors() ->
  case file:list_dir(?RRD_DIRECTORY) of
    {error, Reason} -> 
      Reason;
    {ok, FileList} ->
      lists:map(
        fun(Filename) ->
          [Name|_] = string:tokens(Filename, "."),
          erlang:list_to_atom(Name)
        end, FileList)
  end.
