-module (hermes_sup).
-include ("hermes.hrl").
-behaviour(supervisor).

-export([ start/2,
          init/1,
          stop/1]).

start(Type, Args) ->  
  supervisor:start_link(?MODULE, [Type, Args]).

init([Type, Args]) ->
  io:format("starting ~p with ~p~n", [?MODULE, Args]),
  
  HermesLoggerSup = { hermes_logger,  {hermes_logger, start_link, []}, permanent,2000,worker,[]},
  RrdServerSup    = { erlrrd_sup,     {erlrrd_sup, start_link, []}, permanent, 2000, worker, []},  
  MonServerSup    = { mon_server_sup, {mon_server_sup, start_link, [Args]}, permanent, 2000, worker, []},  
  RestServerSup   = { rest_server,    {rest_app, start, [Type, Args]}, permanent,2000,worker,[]},
  AmbassadorApp   = { ambassador_app, {ambassador_app, start, [Type, Args]}, permanent, 2000, worker, []},
  NagApp          = { nag_app,        {nag_app, start, [Type, Args]}, permanent, 2000, worker, []},
  
  {ok,{_SupFlags = {one_for_one, ?MAXIMUM_RESTARTS, ?MAX_DELAY_TIME},[
    RestServerSup,
    RrdServerSup,
    MonServerSup,
    AmbassadorApp,
    NagApp,
    HermesLoggerSup
  ]}}.
  
stop(Args) ->
  io:format("Stopping hermes~n"),
  lists:map(fun(Term) -> Term:stop(Args) end, [
                                            hermes_logger, 
                                            nag_app,
                                            erlrrd_app, 
                                            mon_server_sup, 
                                            rest_app,
                                            ambassador_app
                                          ]).