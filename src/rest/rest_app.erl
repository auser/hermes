-module (rest_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, Args) ->
  Module = proplists:get_value(module, Args),
  StartArgs = lists:map(fun (Var) -> {ok, Value} = application:get_env(Module, Var), Value end, [port]),
  rest_server_sup:start_link(StartArgs).

stop(_State) -> ok.
