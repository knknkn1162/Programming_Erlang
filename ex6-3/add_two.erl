-module(add_two).
-compile(export_all).
start() ->
  process_flag(trap_exit, true),
  register(add_two, Pid = spawn_link(?MODULE, loop, [])),
  {ok, Pid}.

loop() ->
  receive
    {From, Ref, {request, Int}} ->
      From ! {Ref, {result, Int + 2}},
      loop();
    stop ->
      io:format("exit by normal~n"),
      ok
  end.

stop() ->
  add_two ! stop.

request(Int) ->
  Ref = make_ref(),
  add_two ! {self(), Ref, {request, Int}},
  receive
    {Ref, {result, Result}} ->
      Result;
    {'EXIT', _Pid, Reason} ->
      {error, Reason}
    after 1000 -> timeout
  end.
