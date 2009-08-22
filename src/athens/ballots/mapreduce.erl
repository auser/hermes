-module (mapreduce).
-include ("hermes.hrl").
-export ([submit/2, submit/3]).

submit(MFA, Value) ->
  Nodes = athens:nodes(),
  submit(MFA, Value, Nodes).
  
submit(MFA, Value, NodeList) ->
  S = self(),  
  Acc = 0.0,
  Nodes = ensure_are_nodes(NodeList),
  ?TRACE("Nodes", Nodes),
  Pid = spawn(fun() -> reduce(S, MFA, Value, Acc, Nodes) end),
  receive
    {Pid, R} -> R
  end.
  
reduce(From, MFA, Value, Acc, Nodes) ->
  process_flag(trap_exit, true),
  ReducePid = self(),
  
  lists:foreach(fun(Node) ->
      spawn_link(fun() -> run_mfa(ReducePid, MFA, Node) end)
    end, Nodes),
  
  TotalNumNodes = length(Nodes),
  NumResponses = 0,
  ReducedAvg = collect_reductions(NumResponses, TotalNumNodes, Value, Acc),
  From ! {self(), ReducedAvg}.
  

%%====================================================================
%% REDUCTION
%%====================================================================
%%--------------------------------------------------------------------
%% Function: collect_reductions (C, T, Val, Acc) -> Avg
%% Description: Collect the reductions across the nodes
%%  If the total number of nodes respond, then return the average of
%%    the responses.
%%  If the response comes back that is equal to that of the value
%%    when the election was called, then let's add 1.0 to the acc
%%    value.
%%  If the response comes back differently than the value called 
%%    with the election, then don't add it to the list of responses
%%  If the known nodes send an exit response when calling the MFA
%%    then don't expect a response back from the node
%%--------------------------------------------------------------------
collect_reductions(TotalNumNodes, TotalNumNodes, _, Acc) -> (Acc / TotalNumNodes);
collect_reductions(NumResponses, TotalNumNodes, Val, Acc) ->
  ?TRACE("collect_reductions: ~p, ~p, ~p, ~p~n", [NumResponses, TotalNumNodes, Val, Acc]),
  receive
    {_Node, Val}    -> 
      ?TRACE("Got back value", [Val]),
      collect_reductions(NumResponses + 1, TotalNumNodes, Val, Acc + 1);
    {_Node, Else}  -> 
      ?TRACE("Got back else", [Else]),
      collect_reductions(NumResponses + 1, TotalNumNodes, Val, Acc);
    {'EXIT', _, _}  -> collect_reductions(NumResponses, TotalNumNodes - 1, Val, Acc)
  end.

run_mfa(From, MFA, Node) ->
  [M,F,A] = MFA,
  Val = rpc:call(Node, M, F, A),
  From ! {Node, Val}.

% Avg = mon_server:get_latest_average_for(Monitor)
% 
% Out = ambassador:ask(Fun, Args)

%%====================================================================
%% PRIVATE
%%====================================================================
ensure_are_nodes(List) ->
  [ ensure_is_node(Node) || Node <- List ].

% ensure_is_node(Node) when is_pid(Node) -> erlang:node(Node);
ensure_is_node(Node) when is_atom(Node) -> ensure_is_node(whereis(Node));
ensure_is_node(Node) -> Node.