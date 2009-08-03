-module (monitors).
-export ([get/1, post/2, put/2, delete/2]).

get([]) ->
  [Monitors] = mon_server:list_monitors(),
  {"monitors", Monitors };

get([Monitor]) ->
  Out = erlang:list_to_binary(lists:append([Monitor, " - Add data"])),
  {"monitor", Out };
  
get(_Path) -> {"error", <<"unhandled">>}.

post(_Path, _Data) -> {"error", <<"unhandled">>}.

put(_Path, _Data) -> {"error", <<"unhandled">>}.

delete(_Path, _Data) -> {"error", <<"unhandled">>}.