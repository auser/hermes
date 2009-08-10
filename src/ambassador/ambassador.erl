%%%-------------------------------------------------------------------
%%% File    : ambassador.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Thu Aug  6 20:15:44 PDT 2009
%%%-------------------------------------------------------------------

-module (ambassador).
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export ([echo/1]).

-record(state, {
          port, 
          timeout
        }).
-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
echo(Msg) -> 
  do(echo, Msg).

  
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Cmd) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, Cmd, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(IssuedCmd) ->
  Cmd = case IssuedCmd of
    [] ->
      case application:get_env(ambassador_app, cmd) of
        {ok, Q} -> Q;
        undefined -> "echo -"
      end;
    Else -> Else
  end,
  io:format("----------------------- Command: ~p or ~p~n", [Cmd, IssuedCmd]),
  Timeout = case application:get_env(ambassador_app, timeout) of
    { ok, T } -> T;
    undefined -> 3000
  end,
  process_flag(trap_exit, true),
  Port = erlang:open_port(
    {spawn, Cmd},
    [ {line, 10000}, eof, exit_status, stream ] 
  ),
  {ok, #state{port = Port, timeout = Timeout}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({do, Action, Args }, _From, #state{port = Port, timeout = Timeout } = State) ->
    Line = [ erlang:atom_to_list(Action), " ", Args , "\n"],
    port_command(Port, Line),
    case collect_response(Port, Timeout) of
          {response, Response} -> 
              {reply, { ok, Response }, State};
          { error, timeout } ->
              {stop, port_timeout, State};
          { error, Error } -> 
              {reply, { error, Error  }, State}
    end;
  
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

do(Command, Args) -> 
  case has_newline(Args) of
    true  -> { error, "No newlines" };
    false -> gen_server:call (?MODULE, { do, Command, Args } ) 
  end.


collect_response(Port, Timeout ) ->
    collect_response(Port, [], [], Timeout ).

collect_response( Port, RespAcc, LineAcc, Timeout) ->
    receive
        {Port, {data, {eol, "OK"}}} ->
            {response, lists:reverse(RespAcc)};

        {Port, {data, {eol, Result}}} ->
            Line = lists:reverse([Result | LineAcc]),
            collect_response(Port, [Line | RespAcc], [], Timeout);

        {Port, {data, {noeol, Result}}} ->
            collect_response(Port, RespAcc, [Result | LineAcc], Timeout)

    after Timeout -> 
      timeout
    end.


has_newline([]) -> false;
has_newline(<<>>) -> false;
has_newline([ H |  T]) 
  when is_list(H); is_binary(H) ->
    case has_newline(H) of
      true -> true;
      false -> has_newline(T)
    end;
has_newline([ H | T]) when is_integer(H) ->
  if 
    H =:= $\n -> true;
    true -> has_newline(T)
  end;
has_newline(<<H:8,T/binary>>) ->
  if 
    H =:= $\n -> true;
    true -> has_newline(T)
  end.
