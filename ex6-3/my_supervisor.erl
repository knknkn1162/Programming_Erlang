-module(my_supervisor).
-export([start_link/2, stop/1]).
-export([init/1]).

%%startlink/2 (ProcessName, [{M, F, A, LifeTime}]) where LifeTime = permanent | transient
start_link(Name, ChildSpecList) ->
  register(Name, spawn_link(my_supervisor, init, [ChildSpecList])),
  ok.

init(ChildSpecList) ->
  io:format("my_supervisor : ~p~n", [self()]),
  process_flag(trap_exit, true),
  loop(start_children(ChildSpecList)).

stop(Name) ->
  Name ! {stop, self()},
  receive
    {reply, Reply} ->
      Reply
  end.


start_children([]) ->[];
start_children([HSpec = {M, F, A, _LifeTime}|ChildSpecList]) ->
  case (catch apply(M, F, A)) of
    {ok, Pid} ->
      io:format("Pid :~p~n", [Pid]),
      [{Pid, HSpec}|start_children(ChildSpecList)];
    _ ->
      start_children(ChildSpecList)
  end.


loop(ChildList) ->
  io:format("ChildList: ~p~n", [ChildList]),
  receive
    {'EXIT', Pid, Reason} ->
      io:format("exit ~p for ~p~n", [Pid, Reason]),
      NewChildList = restart_child(Pid, Reason, ChildList),
      loop(NewChildList);
    {stop, From} ->
      From ! {reply, terminate(ChildList)}
    end.

%% meet ex6-3 list1
restart_child(Pid, Reason, ChildList) ->
  %%Data = lists:keysearch(Pid, 1, ChildList), io:format("Data : ~p~n", [Data]),
  {value, {Pid, Spec = {M, F, A, LifeTime}}} = lists:keysearch(Pid, 1, ChildList),
  %% NOT deleted when the transient process is normal exited.
  case {LifeTime, Reason} of
    {transient, normal} ->
      io:format("my_supervisor:restart_child => exit : normal~n"), 
      lists:keydelete(Pid, 1, ChildList);
    %% otherwise
    _Any ->
      io:format("restart for ~p~n", [Reason]),
      {ok, NewPid} = apply(M, F, A),
      [{NewPid, Spec}|lists:keydelete(Pid, 1, ChildList)]
  end.

terminate([{Pid, _}|ChildList]) ->
  exit(Pid, kill),
  terminate(ChildList);
terminate(_ChildList) -> ok.
