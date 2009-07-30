-module (hermes_sup).
-include ("hermes.hrl").
-behaviour(supervisor).

-export([start/2]).
-export([init/1]).

start(Type, Args) -> supervisor:start_link(?MODULE, [Type, Args]).

init([Type, Args]) ->
  
  HermesLoggerSup = { hermes_logger, {hermes_logger, start_link, []}, permanent,2000,worker,[]},
  MonServerSup    = { mon_server, {mon_server, start_link, []}, permanent, 2000, worker, []},
  RestServerSup   = { rest_server, {rest_app, start, [Type, Args]}, permanent,2000,worker,[]},
  
  {ok,{_SupFlags = {one_for_one, ?MAXIMUM_RESTARTS, ?MAX_DELAY_TIME},[
    RestServerSup,
    MonServerSup,
    HermesLoggerSup
  ]}}.