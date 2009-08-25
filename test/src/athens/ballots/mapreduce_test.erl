-module (mapreduce_test).
-include ("hermes.hrl").
-include_lib("eunit/include/eunit.hrl").

setup() ->
  % ttb:tracer(node(), [{file,"trace/ttb"},{process_info,true}]),
  % ttb:tracer(node(), [{file,"trace/ttb"},{process_info,true}]),
  % ttb:p(self(), [call,send,messages,sos,sol]),
  % mon_method([?MODULE, handle_map, []]),
  NodeList = [2,3],
  {ok, Pid} = start_node(1, undefined),
  lists:map(fun(Name) ->
    {ok, P} = start_node(Name, Pid),
    P
    end, NodeList ).

teardown(Servers) ->
  lists:map(fun(Pid) ->
      gen_cluster:cast(Pid, stop)
   end, Servers),
   % ttb:stop(),
   % ttb:format("trace"),
  ok.
  
all_test_() ->
  Nodes = [node1, node2, node3],
  {setup, fun setup/0, fun teardown/1,
    {timeout, 300,
      fun() ->
        NodeList = [ node(Name) || Name <- test_nodes() ],
        ?TRACE("NodeList", [NodeList]),
        F = fun handle_map/2,
        O = mapreduce:submit(F, 1, Nodes),
        ?TRACE("Ran tests: ~p~n", [O])
      end
    }
  }.


%%====================================================================
%% PRIVATE
%%====================================================================
handle_map(Req, From) ->
  ?TRACE("Running generic handle_map", [Req, self()]),
  From ! {self(), Req}.

% mon_method(MFA) ->
%   MS1 = [{'_',[],[{return_trace},{message,{caller}}]}], % dbg:fun2ms(fun(_) -> return_trace(),message(caller()) end),
%   ttb:tpl(MFA, MS1).

start_node(Integer, Seed) ->
  Name = construct_node_name(Integer),
  athens_srv:start_named(Name, {seed, Seed}).

construct_node_name(Integer) ->
  erlang:list_to_atom(lists:flatten([
                                      ["node"], [erlang:integer_to_list(Integer)]%, ["@local"]%, [Hostname]
                                    ])).
  
test_nodes() ->
  {ok, N} = gen_cluster:call(node1, {'$gen_cluster', plist}),
  N.