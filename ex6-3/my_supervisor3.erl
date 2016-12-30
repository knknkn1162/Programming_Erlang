-module(my_supervisor3).
-export([start_link/2, stop/1]).
-export([init/1]).
-export([start_child/4, stop_child/2]).
-define(TIMESPAN, 60).

%%startlink/2 (ProcessName, [{M, F, A, LifeTime}]) where LifeTime = permanent | transient
start_link(Name, ChildSpecList) ->
  register(Name, spawn_link(?MODULE, init, [ChildSpecList])),
  ok.



init(ChildSpecList) ->
  io:format("my_supervisor : ~p~n", [self()]),
  process_flag(trap_exit, true), 
  %% ChildList = {ChildSpecList, RestartLog} 
  loop({start_children(ChildSpecList), []}).

stop(Name) ->
  Name ! {stop, self()},
  receive
    {reply, Reply} ->
      Reply
  end.

start_child(Name, Module, Function, Args) ->
  Name ! {start_child, self(), {Module, Function, Args}},
  receive
    {reply, Pid, Ref} ->
      {Pid, Ref}
  end.

stop_child(Name, Ref) ->
  Name ! {stop_child, Ref}.


start_children([]) ->[];
start_children([HSpec = {M, F, A, _LifeTime}|ChildSpecList]) ->
  case apply_child(M, F, A) of
    {ok, {Pid, Ref}} ->
      io:format("Pid :~p, Ref : ~p~n", [Pid, Ref]),
      [{Pid, Ref, HSpec}|start_children(ChildSpecList)];
    _ ->
      start_children(ChildSpecList)
  end;
start_children(Spec = {_M, _F, _A, _LifeTime}) ->
  hd(start_children([Spec])).

%% meet ex6-3 list3
apply_child(Module, Function, Args) ->
  case (catch apply(Module, Function, Args)) of
  {ok, Pid} -> {ok, {Pid, make_ref()}};
    _ -> error
  end.

%% ChildList = {[{Pid, Ref, HSpec}], RestartLog}
loop(ChildList = {ChildSpecList, RestartLog}) ->
  io:format("ChildList: ~p~n", [ChildList]),
  receive
    {'EXIT', Pid, Reason} ->
      io:format("exit ~p for ~p~n", [Pid, Reason]),
      NewChildList = restart_child(Pid, Reason, ChildList),
      loop(NewChildList);
    {stop, From} ->
      From ! {reply, terminate(ChildSpecList)};
    {start_child, From, {Module, Function, Args}} ->
      NewChildSpecList = start_children([{Module, Function, Args, permanent}]),
      NewChildList = {
        [{ChildPid, Ref, _} = hd(NewChildSpecList)|ChildSpecList]
        , RestartLog
      },
      From ! {reply, ChildPid, Ref},
      loop(NewChildList);
    %% whereis(add_two) 
    {stop_child, Ref} ->
      {value, {Pid, Ref, _}} = lists:keysearch(Ref, 2, ChildSpecList),
      %% Attention) the exit message will be trapped as {'EXIT', Pid, killed}.
      io:format("~p will be exited : Ref => ~p~n", [Pid, Ref]),
      exit(Pid, kill),
      %% deleted next message
      loop(ChildList)
    end.

%% meet ex6-3 list1
restart_child(Pid, Reason, {ChildSpecList, RestartLog}) ->
  io:format("restart_child => ChildSpecList : ~p~n", [ChildSpecList]),
  {value, {Pid, _Ref, Spec = {_M, _F, _A, LifeTime}}} = lists:keysearch(Pid, 1, ChildSpecList),
  ResChildSpecList = lists:keydelete(Pid, 1, ChildSpecList),

  %% NOT restarted when the transient process is normal exited.
  case {LifeTime, Reason} of
    {transient, normal} ->
      io:format("my_supervisor:restart_child => exit : normal~n"), 
      {ResChildSpecList, RestartLog};
    %% when process is terminated(from stop/1 or stop_child/2)
    {_Any, killed} ->
      io:format("my_supervisor:restart_child => exit : killed~n"),
      {ResChildSpecList, RestartLog};
    %% otherwise
    _Any ->
      io:format("restart for ~p~n", [Reason]),
      %% check Restartlog
      case write_log(RestartLog) of
        error ->
          io:format("error : delete, Pid : ~p, Spec : ~p~n", [Pid, Spec]),
          {ResChildSpecList, []};
        NewRestartLog ->
          NewChildSpecList = hd(start_children([Spec])),
          {[NewChildSpecList|ResChildSpecList], NewRestartLog}
      end
  end.
%% for stop/1
terminate([{Pid, _Ref, _}|ChildList]) ->
  exit(Pid, kill),
  terminate(ChildList);
terminate(_ChildList) -> ok.

%% helper for ex6-3 list2
write_log(Log) ->
  case length(Log) =:= 5 of
    false ->
      Log ++ [get_timestamp()];
    true ->
      OldTime = hd(Log),
      NewTime = get_timestamp(), io:format("NewTime : ~p, OldTime ~p ~n", [NewTime, OldTime]),
      case NewTime - OldTime < ?TIMESPAN of
        true ->
          error;
        false ->
          tl(Log) ++ [NewTime]
      end
  end.
%%% get timestamp by sec.
get_timestamp() ->
  {Mega, Sec, _Micro} = os:timestamp(),
  Mega*1000000 + Sec.

