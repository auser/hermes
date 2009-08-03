-module (hermes).
-include ("hermes.hrl").
-behaviour (application).

-export([start/2, stop/1]).

start(Type, Args) ->  
  hermes_sup:start(Type, Args).

stop(_State) -> ok.