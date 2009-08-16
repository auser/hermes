-define(SOFTWARE_NAME, "------------- Hermes ------------").
-define(COPYRIGHT_MESSAGE, "Copyright (C) 2009 Ari Lerner, Nate Murray, Michael Fairchild, CloudTeam").

-define (MAXIMUM_RESTARTS, 10).
-define (MAX_DELAY_TIME, 60).

-define (DEFAULT_NAG_DELAY, 10000).

-define (FMT_MSG (Msg, Args), lists:flatten([?MODULE, ?LINE, io_lib:format(Msg, Args)])).
-define (INFO (Msg, Args),    hermes_logger:info(Msg, Args)).
-define (ERROR (Msg, Args),   hermes_logger:error(Msg, Args)).

-define (TESTING, true).

-define (CONFIG_FILE, case ?TESTING of
  true -> "include/config.cfg";
  false -> "/etc/poolparty/hermes.cfg"
end).

-define (RRD_DIRECTORY, case ?TESTING of
  true -> "test/fixtures";
  false -> "/var/hermes"
end).

-define (LOG_MESSAGE (Message), hermes_logger:append({erlang:localtime(), ?MODULE, ?LINE, Message})).
-define(TRACE(X, M),  io:format(user, "TRACE ~p:~p ~p ~p~n", [?MODULE, ?LINE, X, M])).
-define (DEBUG_LOG (Bool, Message, Opts), 
  case Bool of true -> 
    io:format(Message, Opts); 
  _ -> ok 
  end,
  ?LOG_MESSAGE(io_lib:fwrite(Message, Opts))).
  