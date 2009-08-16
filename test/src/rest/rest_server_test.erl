-module (rest_server_test).
-include ("hermes.hrl").
-include_lib("eunit/include/eunit.hrl").

setup() ->
  application:start(inets),
  {ok, Pid} = rest_server:start_link([{module, hermes}]),
  [Pid].

teardown(_S) ->
  rest_server:stop([]),
  application:stop(inets),
  ok.

mochiweb_start_test_() ->
  {
    setup,
    fun setup/0,
    fun teardown/1,
    fun() ->
      Resp = get_request("http://localhost:9999"),
      % ?TRACE("code:~p~n", [Resp, proplists:get_value(code, Resp)]),
      ?_assertEqual("200", proplists:get_value(code, Resp)),
      HeaderType = proplists:get_value("content-type", proplists:get_value(headers, Resp)),
      ?_assertEqual("text/html", HeaderType),
      ?assertEqual("OK", proplists:get_value(msg, Resp))
    end
  }.
  
get_request(Url) ->
  {ok, {{_HttpVer, Code, Msg}, Headers, Body}} = http:request(get, {Url, [{"User-Agent", "firefox"}]}, [], []),
  [ 
    {msg, Msg},
    {code, Code}, 
    {headers, Headers}, 
    {body, Body}
  ].