%%%-------------------------------------------------------------------
%%% File    : athens.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Wed Aug 19 18:50:53 PDT 2009
%%%-------------------------------------------------------------------

-module (athens).

-export ([  call_election/2,
            nodes/0
         ]).

nodes() ->  athens_srv:nodes().

% Call an election
call_election(MFA, Value) ->
  athens_srv:call_election(MFA, Value).

%%====================================================================
%% PRIVATE
%%====================================================================