-module (mapreduce_test).
-include ("hermes.hrl").
-include_lib("eunit/include/eunit.hrl").

-export ([collect/2]).

setup() ->
  NodeList = [2,3],
  {ok, Pid} = start_node(1, undefined),
  lists:map(fun(Name) ->
    {ok, P} = start_node(Name, Pid),
    P
    end, NodeList ).

teardown(Servers) ->
  lists:map(fun(Pname) -> 
      Pid = whereis(Pname),
      gen_cluster:cast(Pid, stop), 
      unregister(Pname)
   end, Servers),
  ok.
  
all_test_() ->
  Nodes = [node1, node2, node3],
  {setup, fun setup/0, fun teardown/1,
    {timeout, 300, 
      fun() ->
        NodeList = [ node(Name) || Name <- test_nodes() ],
        ?TRACE("NodeList", [NodeList]),
        O = mapreduce:submit([?MODULE, collect, [2, NodeList]], 1, Nodes),
        ?TRACE("Ran tests: ~p~n", [O])
      end
    }
  }.


%%====================================================================
%% PRIVATE
%%====================================================================
collect(Integer, AllNodes) ->  
  ListIndex = list_index(construct_node_name(Integer), AllNodes),
  ?TRACE("----- ", [node(), construct_node_name(Integer), ListIndex, ListIndex rem Integer]),
  ListIndex rem Integer.

start_node(Integer, Seed) ->
  Name = construct_node_name(Integer),
  athens_srv:start_named(Name, {seed, Seed}).

construct_node_name(Integer) ->
  Hostname = net_adm:localhost(),
  erlang:list_to_atom(lists:flatten([
                                      ["node"], [erlang:integer_to_list(Integer)]%, ["@local"]%, [Hostname]
                                    ])).

list_index(ItemVal, List) -> list_index(ItemVal, List, 0).
list_index(ItemVal, [], _) -> undefined;
list_index(ItemVal, [Head|Rest], CurrIndex) when ItemVal =:= Head -> CurrIndex;
list_index(ItemVal, [Head|Rest], CurrIndex) ->
  list_index(ItemVal, Rest, CurrIndex + 1).
  
test_nodes() ->
  {ok, N} = gen_cluster:call(node1, {'$gen_cluster', plist}),
  N.