%%%-------------------------------------------------------------------
%%% File    : athens.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Wed Aug 19 18:50:53 PDT 2009
%%%-------------------------------------------------------------------

-module (athens).

-export ([elect/1]).

% Call an election
elect(Name) ->
  Plist = get_plist(),
  ok.
  
%%====================================================================
%% PRIVATE
%%====================================================================
get_plist() ->
  gen_cluster:plist(athens_srv).