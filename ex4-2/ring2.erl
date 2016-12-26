-module(ring2).
-compile(export_all).

%% N .. Process count
start(N, Msgs) ->
  io:format("start ~p~n", [self()]),
  register(client, self()),
  sends(N, Msgs),
  receiveMessage(Msgs),
  io:format("unregister Pid(~p)~n", [whereis(client)]),
  unregister(client).


loop() ->
  receive
   {message, Msg, N} ->
      io:format("Pid : ~p -> ~p, Message : ~p~n", [self(), N, Msg]),
      sendMessage(N, Msg)
  after 0 -> io:format("exit : ~p~n", [self()]), exit(normal)
  end,
  loop().

%% private API

spawns(0) ->
  [];
spawns(N) ->
  [spawn(?MODULE, loop, [])|spawns(N-1)].

sendMessage(0, Msg) ->
  io:format("*complete sending messages!!*~n"),
  client ! {reply, Msg}; 
sendMessage(N, Msg) ->
  Pid = spawn(?MODULE, loop, []), 
  Pid ! {message, Msg, N-1}.


sends(_, []) ->
  ok;
sends(N, [HMsg|TMsgs]) ->
  sendMessage(N, HMsg),
  sends(N, TMsgs).


receiveMessage([]) -> io:format("***complete receiving messages***~n");
receiveMessage([HMsg|TMsgs]) ->
  receive
    {reply, HMsg} ->
      io:format("message send => ~p (res ~p)~n", [HMsg, TMsgs])
  end,
  receiveMessage(TMsgs). 
