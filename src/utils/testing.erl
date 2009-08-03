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
  Fixtures = [cpu, memory, disk],
  {Mega, Secs, _} = now(),
  StartTime = Mega*1000000 + Secs,
  Ras = lists:append([" --start ", erlang:integer_to_list(StartTime), 
                      " DS:cpu:GAUGE:600:0:1250000 RRA:AVERAGE:0.5:1:24 RRA:LAST:0.5:6:10"]),
  % Create the rrds
  lists:map(
    fun(Module) ->
      Meth = lists:append([?RRD_DIRECTORY, "/", erlang:atom_to_list(Module), ".", "rrd", Ras]),
      erlrrd:create(Meth)
    end, Fixtures),
  % Update the rrds with some fake data
  lists:map(
    fun(Module) -> 
      io:format("Creating fixture data for ~p~n", [Module]),
      Values = [
        "0.5","0.3","0.2","0.1","0.1","0.4",
        "0.7","0.9","1.1","0.9","0.8","0.5"
      ],
      % Update with the list of values
      lists:zipwith(
        fun(Value, Count) ->
          M = lists:append([
            ?RRD_DIRECTORY, "/", erlang:atom_to_list(Module), ".", "rrd", % File test/fixtures/fixture.rrd
            " ", % Space
            erlang:integer_to_list(StartTime + 600*Count), ":", Value % Value -> Time:Value
          ]),
          erlrrd:update(M)
        end, Values, lists:seq(1, erlang:length(Values)))
    end, Fixtures).