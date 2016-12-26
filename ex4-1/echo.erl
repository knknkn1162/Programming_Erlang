-module(echo).
-export([start/0, print/1, stop/0]).
-export([loop/0]).
%%-compile(export_all).
-define(TIMEOUT, 1000).

start() ->
  register(server, spawn(?MODULE, loop, [])).
  

print(Msg) ->
  Ref = make_ref(),
  server ! {self(), Ref, {print, Msg}},
  receive
    {Ref, {reply, done}} ->
      ok
  after ?TIMEOUT -> erlang:error(timeout)
  end.

stop() ->
  Ref = monitor(process, server),
  exit(whereis(server), shutdown),
  io:format("Ref ~p~n", [Ref]),
  receive
    {'DOWN', Ref, process, Pid, shutdown} ->
      io:format("Down Pid : ~p~n", [Pid]),
      ok
    %%Any -> io:format("~p~n", [Any])
  after ?TIMEOUT -> erlang:error(timeout)
  end.
  %%unregister(server),
%% API
%%% private API for server 
loop() ->
  receive
    {From, Ref, {print, _Msg}} ->
      From ! {Ref, {reply, done}},
      loop()
    %%{From, Ref, stop} ->
      %%io:format("stop~n"), ok
  end.


