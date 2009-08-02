-module (hermes).
-include ("hermes.hrl").
-behaviour (application).

-export([start/2, stop/1]).

start(Type, _Args) ->
  {ok, Config} = config:read(),
  hermes_sup:start(Type, Config).

stop(_State) -> ok.