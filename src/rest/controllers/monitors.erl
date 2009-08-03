-module (monitors).
-export ([get/1, post/2, put/2, delete/2]).

get([]) ->
  [Monitors] = mon_server:list_monitors(),
  MonitorData = lists:map(
    fun(Monitor) ->
      Vals = handle_get_monitor_over_time(erlang:atom_to_list(Monitor), 600),
      {struct, [{Monitor, Vals}]}
    end,
    Monitors),
  {"monitors", MonitorData };

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
change_to_float(Int)      -> erlang:float_to_list(Int).

handle_get_monitor_over_time(Monitor, Time) ->
  {_ModAtom, Vals} = mon_server:get_average_over(erlang:list_to_atom(Monitor), Time),
  PrintableVals = lists:map(fun(V) ->
      {A, Int} = V,
      {struct, [{A, utils:turn_binary(change_to_float(Int))}]}
    end, Vals),
  PrintableVals.