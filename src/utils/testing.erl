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
 
%%--------------------------------------------------------------------
%% Function: create_fixture_rrds () -> {ok}
%% Description: Create fixture rrd files in the fixtures directory
%%--------------------------------------------------------------------
create_fixture_rrds() ->
  Fixtures = [cpu, memory],
  Ras = " DS:cpu:GAUGE:600:0:1250000 RRA:AVERAGE:0.5:1:24 RRA:LAST:0.5:6:10",
  % Create the rrds
  lists:map(
    fun(Module) ->
      Meth = lists:append([?RRD_DIRECTORY, "/", erlang:atom_to_list(Module), ".", "rrd", Ras]),
      erlrrd:create(Meth)
    end, Fixtures),
  % Update the rrds with some fake data
  lists:map(
    fun(Module) -> 
      io:format("Creating fixture data for ~p~n", [Module])
    end, Fixtures).