-module (thrift_ambassador).
-include ("hermes_thrift.hrl").

-export ([  
          start/1,
          get_pid/0,
          call/3,
          stop/0
         ]).

start(Args) ->
  ThriftPort  = proplists:get_value(proto_port, Args),
  Module      = proplists:get_value(module, Args),
  thrift_socket_server:start([
    {port, ThriftPort},
    {name, ambassador},
    {service, ambassador_thrift},
    {handler, Module},
    {socket_opts, [{recv_timeout, infinity}]}]),
    
  {ok, HostName} = get_hostname(),
  loudmouth:banner("Started thrift", [
    {"thrift_port", erlang:integer_to_list(ThriftPort)},
    {"thrift client hostname", HostName}
  ]),
  
  thrift_client:start_link(HostName, ThriftPort, ambassador_thrift),
  {ok}.

stop() ->
  thrift_socket_server:stop(get_hostname()),
  ok.

call(Pid, Func, Msg) ->
  thrift_client:call(Pid, Func, Msg).
  
%%--------------------------------------------------------------------
%% Function: get_hostname () -> HostName
%% Description: Quick accessor to local node's hostname
%% TODO: Make a commandline-passable-option
%%--------------------------------------------------------------------
get_hostname() -> inet:gethostname().

get_pid() -> erlang:whereis(ambassador).