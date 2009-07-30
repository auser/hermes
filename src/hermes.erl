-module (hermes).
-include ("hermes.hrl").
-behaviour (application).

-export([start/2, stop/1]).

start(Type, _Args) ->
  hermes_sup:start(Type, [{module, ?MODULE}]).

stop(_State) -> ok.