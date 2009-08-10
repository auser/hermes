-module (ambassador_sup).

-export([start_link/1]).

-behavior(supervisor).

-export([init/1]).

%% @spec start_link() ->  Result
%%   Result = {ok,Pid} | ignore | {error,Error}
%%     Pid = pid()
%%     Error = {already_started,Pid} | shutdown | term()
start_link(Cmd) ->
  supervisor:start_link(ambassador_sup, Cmd).

init(Cmd) -> 
  { 
    ok, 
    { {one_for_one, 5, 10 }, [ { ambassador, { ambassador, start_link, [Cmd] }, permanent, 3000, worker, [ ambassador ] }]}
  }.
