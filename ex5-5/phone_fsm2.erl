%% connected process ID version.
-module(phone_fsm2).
-compile(export_all).
-define(TIMEOUT, 5000).
-define(MAX_CALL, 10).
%% for client
%%% start server
start() ->
  register(phone, spawn(fun() -> idle() end)).

terminate() ->
  Ref = monitor(process, phone),
  exit(whereis(phone), shutdown),
  receive
    {'DOWN', Ref, process, _Pid, shutdown} ->
      io:format("fsm terminate.:Ref => ~p, _Pid => ~p~n", [Ref, _Pid])
  end.

%% client
gen() -> spawn(?MODULE, loop, []).
loop() -> 
  receive
    {call, Pid} -> incomming(Pid)
  end,
  loop().

incomming(Pid) ->
  phone ! {incomming, Pid}.

%% client events.

stop_ringing() ->
  phone ! other_on_hook, ok.

off_hook(Pid) ->
  phone ! {off_hook, Pid},ok.

start_tone() ->
  phone ! off_hook,ok.


call(Pid) ->
  phone ! {call, Pid}, ok.



bye() ->
  phone ! on_hook, 
  ok.

%% helper for client


 
%% state
idle() ->
  io:format("current state is idle~n"),
  receive
    {incomming, From} ->
      ringing(From, 0);
    off_hook ->
      dial();
    _ -> idle()
  end.
ringing(_, Times) when Times >= ?MAX_CALL ->
  io:format("mmm...~n"),
  idle();
ringing(From, Times) -> 
  io:format("trrr..trrr.. <= ringing ~p times~n", [Times+1]),
  receive 
    {off_hook, From} ->
      connected(From);
    other_on_hook ->
      io:format("other_on_hook : ~p~n", [From]),
      idle()
  after ?TIMEOUT -> 
    flush(), ringing(From, Times+1)
 end.

connected(From) ->
  io:format("current state is connected for ~p~n", [From]),
  receive
    on_hook ->
      io:format("bye ~p.~n", [From]),
      idle();
    _ -> io:format("you can't conected~n"), connected(From)
  end.
  

dial() ->
  io:format("current state is dial~n"),
  receive
    on_hook ->
      idle();
    {call, Pid} ->
      io:format("I'll call on ~p~n", [Pid]),
      case is_process_alive(Pid) of
        true ->
          connected(Pid);
        false ->
          io:format("error : process dead!~n"),
          dial()
      end;
    _ -> dial()
  end.


%% helper for server
flush() ->
  receive
    _ -> flush()
  after 0 -> ok
  end.
