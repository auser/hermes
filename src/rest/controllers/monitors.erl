-module (monitors).
-export ([get/1, post/2, put/2, delete/2]).

get([]) ->
  [Monitors] = mon_server:list_monitors(),
  {"monitors", Monitors };

get([Monitor]) ->
  {_ModAtom, Vals} = mon_server:get_average_over(erlang:list_to_atom(Monitor), 1000),
  PrintableVals = lists:map(fun(V) ->
      {A, Int} = V,
      [A, erlang:float_to_list(Int)]
    end, Vals),
  Out = erlang:list_to_binary(lists:append([Monitor, PrintableVals])),
  {"monitor", Out };
  
get(_Path) -> {"error", <<"unhandled">>}.

post(_Path, _Data) -> {"error", <<"unhandled">>}.

put(_Path, _Data) -> {"error", <<"unhandled">>}.

delete(_Path, _Data) -> {"error", <<"unhandled">>}.