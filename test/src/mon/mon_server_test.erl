-module (mon_server_test).
-include ("hermes.hrl").
-include_lib("eunit/include/eunit.hrl").

setup() ->
  mon_server:start_link([]).

teardown(_Servers) ->
  mon_server:stop(),
  ok.
  
all_test_() ->
  {setup, fun setup/0, fun teardown/1,
    {timeout, 300,
      fun() ->
        test_get_monitors(),
        test_get_related_monitors()
      end
    }
  }.
  
%%====================================================================
%% FIXTURES 
%% test/fixtures/supermonitors/mons
%%====================================================================

test_get_monitors() ->
  ExpectedMonitors = [cpu, memory, disk],
  M = mon_server:list_monitors(),
  ?assertEqual(ExpectedMonitors, proplists:get_keys(M)).
  
test_get_related_monitors() ->
  ?assertEqual(['cpu-free', 'cpu-idle', 'cpu-used'], mon_server:list_related_monitors(cpu))
  ,?assertEqual([], mon_server:list_related_monitors(nothing_here))
  ,?assertEqual(['memory-free', 'memory-idle', 'memory-used'], mon_server:list_related_monitors(memory))
  ,?assertEqual(['cpu-idle'], mon_server:list_related_monitors(cpu, idle))
  .