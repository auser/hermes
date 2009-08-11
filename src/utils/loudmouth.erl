-module (loudmouth).

-export ([banner/1]).

%%--------------------------------------------------------------------
%% Function: banner (Lines) -> ok
%% Description: 
%% Lines = [{"node", "1"},{"port", "8991"}]
%%--------------------------------------------------------------------
banner(Lines) ->
  DescrLen = lists:max([length(K) || {K, _V} <- Lines]),
  lists:foreach(fun (T) -> 
      case T of        
        {K, V}  -> io:format("~-" ++ integer_to_list(DescrLen) ++ "s: ~s~n", [K, V]);
        E       -> io:format("~s~n", [E])
      end
    end, Lines),
  BannerStripes = string:copies("-", DescrLen*3),
	io:format("~s~n", [BannerStripes]),
  io:nl().  