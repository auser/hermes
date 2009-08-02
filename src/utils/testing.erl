-module (testing).
-include ("hermes.hrl").
-compile (export_all).

% f(). Str = "name=baskinrobbins&boxes=full". testing:turn_equal_to_json(Str).
turn_equal_to_json(Str) ->
  Set = string:tokens(Str, "&"),
  ArrSet = lists:map(
    fun(Line) -> 
      string:tokens(Line, "=")
    end, Set),
  lists:map(fun([K|V]) -> "{'"++K++"':'"++erlang:list_to_binary(V)++"'}" end, ArrSet).
 
create_fixture_rrds() ->
  Ras = "DS:cpu:GAUGE:600:0:1250000 RRA:AVERAGE:0.5:1:24 RRA:LAST:0.5:6:10",
  lists:map(
    fun(Module) ->
      Meth = io_lib:fwrite("~w/~w.rrd", [?RRD_DIRECTORY, erlang:atom_to_list(Module)]),
      io:format("Using ~p to create a new archive~n", [Meth]),
      erlrrd:create(Meth)
    end, [cpu, memory]).