-module (mapreduce_test).
-include ("hermes.hrl").
-include_lib("eunit/include/eunit.hrl").

-export ([test_athens_response_function/2]).

setup() ->
  % ttb:tracer(node(), [{file,"trace/ttb"},{process_info,true}]),
  % ttb:tracer(node(), [{file,"trace/ttb"},{process_info,true}]),
  % ttb:p(self(), [call,send,messages,sos,sol]),
  % mon_method([?MODULE, handle_map, []]),
  start_n_nodes(3).

teardown(Servers) ->
  lists:map(fun({_Name, Pid}) ->
      gen_cluster:cast(Pid, stop)
   end, Servers),
   % ttb:stop(),
   % ttb:format("trace"),
  ok.
  
all_test_() ->
  {setup, fun setup/0, fun teardown/1,
    {timeout, 300,
      fun() ->
        NodeList = test_nodes(),
        ?TRACE("NodeList", [NodeList]),
        O = athens_srv:call_election(athens_srv, blah, 1, NodeList),
        % O = mapreduce:submit(?MODULE, F, 1, test_nodes()),
        ?TRACE("Ran tests: ~p~n", [O])
      end
    }
  }.


test_athens_response_function(Num, FromPid) ->
  ?TRACE("in test_athens_response_function", []),
  FromPid ! {self(), Num}.
    
%%====================================================================
%% PRIVATE
%%====================================================================
start_node(Integer, Seed) ->
  Name = construct_node_name(Integer),
  athens_srv:start_named(Name, {seed, Seed}).

construct_node_name(Integer) ->
  erlang:list_to_atom(lists:flatten([
                                      ["node"], [erlang:integer_to_list(Integer)]%, ["@local"]%, [Hostname]
                                    ])).


start_n_nodes(N) ->
  {ok, OrigPid} = start_node(1, undefined),
  O = lists:map(fun(Name) -> 
    {ok, P} = start_node(Name, OrigPid),
    {Name, P}
  end, lists:seq(2, N) ),
  lists:flatten([{node1, OrigPid},O]).

test_nodes() ->
  {ok, N} = gen_cluster:call(node1, {'$gen_cluster', plist}),
  N.