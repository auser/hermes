-module (mapreduce).
-include ("hermes.hrl").
-export ([submit/3, submit/4]).

submit(M, F, Value) ->
  Nodes = athens:nodes(),
  submit(M, F, Value, Nodes).
  
submit(M, F, A, NodeList) ->
  S = self(),
  Acc = 0.0,
  Nodes = ensure_are_nodes(NodeList),
  Pid = spawn(fun() -> 
    reduce(S, M, F, A, Acc, Nodes)
  end)
  ,
  receive
    {_Pid, R} -> R
  end.
  
reduce(From, M, F, ComparisonValue, Acc0, Nodes) ->
  process_flag(trap_exit, true),
  ReducePid = self(),
  
  lists:foreach(fun(Node) ->
      spawn_link(Node, fun() -> run_fun(ReducePid, [M,F, [ComparisonValue]]) end)
    end, Nodes),

  TotalNumNodes = length(Nodes),
  Dict0 = dict:new(),
  % Collect the map reduce
  Dict1 = collect_reductions(TotalNumNodes, Dict0),
  ?TRACE("Got back from collect_reductions", [TotalNumNodes, Acc0]),
  % Reduce the values
  Acc = dict:fold(fun(Node, Value, A) ->
      ?TRACE("From Node V = ", [Node, Value]),
      O = case Value of
        ComparisonValue -> 1.0;
        _Else -> 0.0
      end,
      A + O
    end, Acc0, Dict1),
  % Compute the average over the nodes
  TotalAverage = Acc / TotalNumNodes,
  From ! {self(), TotalAverage}.

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
%%  If the known nodes send an exit response when calling the Fun
%%    then don't expect a response back from the node
%%--------------------------------------------------------------------
collect_reductions(0, Dict) -> Dict; 
collect_reductions(N, Dict) -> 
  receive
    {Node, Val} -> 
    ?TRACE("collect_reductions", [N, Node, Val]),
      case dict:is_key(Node, Dict) of 
        true -> 
          Dict1 = dict:append(Node, Val, Dict), 
          collect_reductions(N, Dict1); 
        false -> 
          Dict1 = dict:store(Node, Val, Dict), 
          collect_reductions(N, Dict1) 
      end; 
    {'EXIT', _, Why} ->
      ?TRACE("EXIT", [Why]),
      collect_reductions(N-1, Dict) 
  end.

run_fun(From, MFA, Node) ->
  [M,F,A] = MFA,
  ?TRACE("Running ", [M,F,A]),
  {ok, Output} = rpc:call(Node,M,F,A),
  From ! Output.
  
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