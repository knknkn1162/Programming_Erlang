-module(phone_fsm).
-compile(export_all).
-define(TIMEOUT, 5000).
-define(MAX_CALL, 10).
%% for client
start() ->
  register(phone, spawn(fun() -> idle() end)),
  r(), ok.

r() ->
  spawn(fun() -> start_ringing() end).

terminate() ->
  Ref = monitor(process, phone),
  exit(whereis(phone), shutdown),
  receive
    {'DOWN', Ref, process, _Pid, shutdown} ->
      io:format("fsm terminate.:Ref => ~p, _Pid => ~p~n", [Ref, _Pid])
  end.


start_ringing() ->
  receive
    _ -> ok
  after ?TIMEOUT -> phone ! {incomming, "888-8888"}
  end.
  %%start_ringing().


stop_ringing() ->
  phone ! {other_on_hook, "888-8888"}, ok.

off_hook(Number) ->
  phone ! {off_hook, Number},ok.

start_tone() ->
  phone ! off_hook,ok.


call(Number) ->
  phone ! {call, Number}, ok.
  
bye() ->
  io:format("bye~n"),
  phone ! on_hook, 
  ok.


%% state
idle() ->
  io:format("current state is idle~n"),
  receive
    {incomming, Number} ->
      ringing(Number, 0);
    off_hook ->
      dial()
  end.
ringing(_, Times) when Times >= ?MAX_CALL ->
  io:format("mmm...~n"),
  idle();
ringing(Number, Times) -> 
  io:format("trrr..trrr.. <= "), io:format("ringing ~p times~n", [Times+1]),
  receive 
    {off_hook, Number} ->
      connected(Number);
    {other_on_hook, Number} ->
      io:format("other_on_hook : ~p~n", [Number]),
      idle()
  after ?TIMEOUT ->  ringing(Number, Times+1)
 end.

connected(Number) ->
  io:format("current state is connected for ~p~n", [Number]),
  receive
    on_hook ->
      idle()
  end.
  
  
dial() ->
  io:format("current state is dial~n"),
  receive
    on_hook ->
      idle();
    {call, Number} ->
      io:format("I call on ~p", [Number]),
      connected(Number)
  end.
