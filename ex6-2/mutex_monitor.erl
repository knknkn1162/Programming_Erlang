%% erlang:monitor version
-module(mutex_monitor).
-export([start/0, stop/0]).
-export([wait/0, signal/0]).
-export([init/0, flush/1]).

%% for client
start() ->
  register(mutex, spawn(?MODULE, init, [])).

stop() ->
  mutex ! stop.


%% event
wait() ->
  mutex ! {wait, self()},
  receive
   ok -> ok
  end.

signal() ->
  mutex ! {signal, self()},
  ok.

%% trigger function
init() ->
  free().

%% state
free() ->
  receive
    {wait, Pid} ->
      Pid ! ok,
      busy(Pid);
    stop ->
      terminate()
  end.

busy(Pid) ->
  Ref = erlang:monitor(process, Pid),
  receive
    {signal, Pid} ->
      erlang:demonitor(Ref),
      free();
    {'DOWN', Ref, process, Pid, Reason} ->
      io:format("down ~p for ~p~n => change free state", [Pid, Reason]),
      % trash {wait Pid} message because of the waiting process dead.
      flush(Pid),
      % change status to free.
      free()
  end.

%% flush {wait, Pid} and shed message
flush(Pid) ->
  receive
    {wait, Pid} -> io:format("wait ~p~n", [Pid]),flush(Pid)
  after 0 ->
    ok
  end.


terminate() ->
  receive
    {wait, Pid} ->
      exit(Pid, kill),
      terminate()
  after 0 -> ok
  end.

