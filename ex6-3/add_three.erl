-module(add_three).
-compile(export_all).
start() ->
  process_flag(trap_exit, true),
  register(add_three, Pid = spawn_link(?MODULE, loop, [])),
  {ok, Pid}.

loop() ->
  receive
    {From, Ref, {request, Int}} ->
      From ! {Ref, {result, Int + 3}},
      loop();
    stop ->
      io:format("exit by normal~n"),
      ok
  end.

stop() ->
  add_three ! stop.

request(Int) ->
  Ref = make_ref(),
  add_three ! {self(), Ref, {request, Int}},
  receive
    {Ref, {result, Result}} ->
      Result;
    {'EXIT', _Pid, Reason} ->
      {error, Reason}
    after 1000 -> timeout
  end.
