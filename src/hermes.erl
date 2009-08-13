-module (hermes).
-include ("hermes.hrl").
-behaviour (application).

-export([start/2, stop/1, stop/0]).

start(Type, Args) ->  
  hermes_sup:start(Type, Args).

stop() -> stop([]).
  
stop(State) -> 
  hermes_sup:stop(State),
  ok.