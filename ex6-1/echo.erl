-module(echo).
-export([start/0, print/1, stop/0]).
-export([loop/0, init/0]).
%%-compile(export_all).
-define(TIMEOUT, 1000).

start() ->
  register(server, spawn_link(?MODULE, init , [])).
  
init() ->
  process_flag(trap_exit, true),
  loop().

print(Msg) ->
  Ref = make_ref(),
  server ! {self(), Ref, {print, Msg}},
  receive
    {Ref, {reply, done}} ->
      ok
  after ?TIMEOUT -> erlang:error(timeout)
  end.

stop() ->
  exit(reason).
%% API
%%% private API for server 
loop() ->
  receive
    {From, Ref, {print, _Msg}} ->
      From ! {Ref, {reply, done}},
      loop();
    {'EXIT', Pid, Reason} ->
      io:format("exit ~p for ~p~n", [Pid, Reason])
    %%{From, Ref, stop} ->
      %%io:format("stop~n"), ok
  end.


