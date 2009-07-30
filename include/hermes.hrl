-define(SOFTWARE_NAME, "------------- Hermes ------------").
-define(COPYRIGHT_MESSAGE, "Copyright (C) 2009 Ari Lerner, Nate Murray, Michael Fairchild, CloudTeam").

-define (MAXIMUM_RESTARTS, 10).
-define (MAX_DELAY_TIME, 60).

-define (DEFAULT_CONFIG, [
					{port, 8642},
					{module, hermes}
				]).
				
-define (LOG_MESSAGE (Message), hermes_logger:append({erlang:localtime(), ?MODULE, ?LINE, Message})).
-define (DEBUG_LOG (Bool, Message, Opts), 
  case Bool of true -> 
    io:format(Message, Opts); 
  _ -> ok 
  end,
  ?LOG_MESSAGE(io_lib:fwrite(Message, Opts))).