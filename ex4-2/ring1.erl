-module(ring1).
%%-compile([start/3]).
-compile(export_all).

%% N .. Process count
start(N, Msgs) ->
 io:format("start ~p~n", [self()]),
  register(client, self()),
  Pids = spawns(N),
  io:format("~p~n", [Pids ++ [hd(Pids)]]),
  sends(Pids ++ [hd(Pids)], Msgs),
  receiveMessage(Msgs),
  io:format("unregister Pid(~p)~n", [whereis(client)]),
  unregister(client).


loop() ->
  receive
   {message, Msg, Pids} ->
      io:format("Pid : ~p -> ~p, Message : ~p~n", [self(), Pids, Msg]),
      sendMessage(Pids, Msg)
  after 1000 -> io:format("exit : ~p~n", [self()]), exit(normal)
  end,
  loop().

%% private API

spawns(0) ->
  [];
spawns(N) ->
  [spawn(?MODULE, loop, [])|spawns(N-1)].

sendMessage([], Msg) ->
  io:format("*complete sending messages!!*~n"),
  client ! {reply, Msg}; 
sendMessage(Pids = [HPid|TPids], Msg) ->
  io:format("{message, ~p, ~p} of ~p~n",[Msg, TPids, Pids]),
   HPid ! {message, Msg, TPids}.

sends(_, []) ->
  ok;
sends(Pids, [HMsg|TMsgs]) ->
  sendMessage(Pids,HMsg),
  sends(Pids, TMsgs).


receiveMessage([]) -> io:format("***complete receiving messages***~n");
receiveMessage([HMsg|TMsgs]) ->
  receive
    {reply, HMsg} ->
      io:format("message send => ~p (res ~p)~n", [HMsg, TMsgs])
  end,
  receiveMessage(TMsgs). 
