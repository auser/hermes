-module (athens_test).
-include ("hermes.hrl").
-include_lib("eunit/include/eunit.hrl").

setup() ->
  [].

teardown(_Servers) ->
  ok.
  
all_test_() ->
  {setup, fun setup/0, fun teardown/1,
    {timeout, 300,
      fun() ->
        ?assert(false)
      end
    }
  }.
