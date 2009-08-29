-module (monitors).
-include ("hermes.hrl").
-export ([get/1, post/2, put/2, delete/2]).

get([]) ->
  Monitors = mon_server:list_monitors(),
  MonitorData = lists:map(
    fun(Monitor) ->
      case Monitor of
        unknown_monitor -> unknown_monitor;
        _Else ->
          O = handle_get_monitor_over_time(Monitor, 600),
          ?TRACE("O (in get)", O),
          O
      end
    end,
    Monitors),
  % ?INFO("MonitorData: ~p~n", [MonitorData]),
  {?MODULE, {struct, MonitorData}};

get(["list"]) ->
  Monitors = mon_server:list_monitors(),
  JsonMonitors = lists:map(fun({Mon, Types}) ->
      {Mon, lists:map(fun(T) -> list_to_binary(T) end, Types)}
    end, Monitors),
    
  {?MODULE, {struct, JsonMonitors}};

get([Monitor]) ->
  Vals = handle_get_monitor_over_time(Monitor, 600),
  {Monitor, Vals};
 
get([Monitor, Time]) -> 
  Vals = handle_get_monitor_over_time(Monitor, erlang:list_to_integer(Time)),
  {Monitor, Vals};
  
get(_Path) -> {"error", <<"unhandled">>}.

post(_Path, _Data) -> {"error", <<"unhandled">>}.

put(_Path, _Data) -> {"error", <<"unhandled">>}.

delete(_Path, _Data) -> {"error", <<"unhandled">>}.

%%====================================================================
%% Private methods
%%====================================================================
change_to_float("nan")  -> 0;
change_to_float([])     -> 0.0;
change_to_float(Int)    -> Int.

handle_get_monitor_over_time(MonitorAtom, Time) ->  
  Vals = mon_server:get_average_over(MonitorAtom, Time),
  PrintableVals = lists:map(fun(V) ->
      {A, ListOfAtoms} = V,
      O = lists:map(fun({T, B}) -> 
          {T, utils:turn_binary(change_to_float(B))}
        end, ListOfAtoms),
      {erlang:list_to_atom(A), {struct, O}}
    end, Vals),
  PrintableVals.