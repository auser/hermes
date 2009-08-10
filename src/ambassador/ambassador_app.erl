-module (ambassador_app).

-export([start/0, stop/0]).
-behavior(application).
-export([start/2, stop/1]).


start() -> 
  application:start(ambassador_app).
start(_Type, Args) -> 
  ambassador_sup:start_link(Args).
stop() -> application:stop(ambassador_app).
stop(_State) -> ok. 
