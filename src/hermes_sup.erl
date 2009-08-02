-module (hermes_sup).
-include ("hermes.hrl").
-behaviour(supervisor).

-export([start/2]).
-export([init/1]).

start(Type, Args) ->  
  supervisor:start_link(?MODULE, [Type, Args]).

init([Type, Config]) ->
    
  HermesLoggerSup = { hermes_logger,  {hermes_logger, start_link, []}, permanent,2000,worker,[]},
  RrdServerSup    = { erlrrd_sup,     {erlrrd_sup, start_link, []}, permanent, 2000, worker, []},
  
  {ok, Monitors} = config:get(monitors, Config),
  MonServerSup    = { mon_server,     {mon_server, start_link, [Monitors]}, permanent, 2000, worker, []},
  
  RestServerSup   = { rest_server,    {rest_app, start, [Type, Config]}, permanent,2000,worker,[]},
  
  {ok,{_SupFlags = {one_for_one, ?MAXIMUM_RESTARTS, ?MAX_DELAY_TIME},[
    RestServerSup,
    RrdServerSup,
    MonServerSup,
    HermesLoggerSup
  ]}}.