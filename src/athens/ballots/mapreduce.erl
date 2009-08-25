-module (mapreduce).
-include ("hermes.hrl").
-export ([submit/2, submit/3]).

submit(Fun, Value) ->
  Nodes = athens:nodes(),
  submit(Fun, Value, Nodes).
  
submit(Fun, Args, NodeList) ->
  S = self(),  
  Acc = 0.0,
  Nodes = ensure_are_nodes(NodeList),
  ?TRACE("Nodes", Nodes),
  % Pid = spawn(fun() -> 
    reduce(S, Fun, Args, Acc, Nodes)
  % end),
  ,
  receive
      {_Pid, R} -> R
    end.
  
reduce(From, Fun, ComparisonValue, Acc0, Nodes) ->
  process_flag(trap_exit, true),
  ReducePid = self(),
  
  lists:foreach(fun(Node) ->
      spawn_link(fun() -> run_fun(ReducePid, Fun, ComparisonValue) end)
    end, Nodes),

  TotalNumNodes = length(Nodes),
  % NumResponses = 0,
  % Dict0 = dict:new(),
  % {response, Arr} = collect_replies(TotalNumNodes, Args, Dict0),
  % ReducedAvg = proplists:get_value(average, Arr),
  % Tot = proplists:get_value(total_responses, Arr),
  % ?TRACE("ReducedAvg from #", [ReducedAvg, Tot]),
  % From ! {self(), ReducedAvg}.

  %% make a dictionary to store the Keys 
  Dict0 = dict:new(), 
  %% Wait for N Map processes to terminate 
  Dict1 = collect_replies(TotalNumNodes, Dict0),
  ?TRACE("Got back from collect_replies", [TotalNumNodes, Acc0]),
  Acc = dict:fold(fun(Node, Value, A) ->
      ?TRACE("From Node V = ", [Node, Value]),
      O = case Value of
        ComparisonValue -> 1.0;
        Else -> 0.0
      end,
      A + O
    end, Acc0, Dict1),
  ?TRACE("Acc", [Acc]),
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
% collect_reductions(TotalNumNodes, TotalNumNodes, _, Acc) ->  (Acc / TotalNumNodes);
%   
% collect_reductions(NumResponses, TotalNumNodes, Val, Acc) ->
%   ?TRACE("collect_reductions", [self, self(), num_responses, NumResponses, total_num_of_nodes, TotalNumNodes, val, Val, acc, Acc]),
%   receive
%     Val ->
%       ?TRACE("Got back value", [Val, self()]),
%       collect_reductions(NumResponses + 1, TotalNumNodes, Val, Acc + 1);
%     {'EXIT', _, _}  ->
%         ?TRACE("got EXIT", [A, B, self()]),
%         collect_reductions(NumResponses + 1, TotalNumNodes, Val, Acc);
%     Else  ->
%       ?TRACE("Got back else", [Else]),
%       collect_reductions(NumResponses + 1, TotalNumNodes, Val, Acc)
%   end.

collect_replies(0, Dict) -> Dict; 
collect_replies(N, Dict) -> 
  receive
    {Node, Val} -> 
    ?TRACE("collect_replies", [N, Node, Val]),
      case dict:is_key(Node, Dict) of 
        true -> 
          Dict1 = dict:append(Node, Val, Dict), 
          collect_replies(N, Dict1); 
        false -> 
          Dict1 = dict:store(Node, Val, Dict), 
          collect_replies(N, Dict1) 
      end; 
    {'EXIT', _, Why} ->
      ?TRACE("EXIT", [Why]),
      collect_replies(N-1, Dict) 
  end.

run_fun(From, Fun, Args) ->    
  Fun(Args, From).
  
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