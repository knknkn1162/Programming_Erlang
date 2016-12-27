-module(log_handler).
-compile(export_all).

init(File) ->
  {ok, Fd} = file:open(File, write), Fd.

terminate(Fd) -> 
  {ok, Filename} = file:pid2name(Fd),
  file:close(Fd), 
  Filename.

handle_event({Action, Id, Event}, Fd) ->
  {MegaSec, Sec, MicroSec} = erlang:now(),
  io:format(Fd, "~w, ~w,  ~w, ~w, ~w, ~p~n",
    [MegaSec, Sec, MicroSec, Action, Id, Event]),
  Fd;

handle_event(_, Fd) ->
  Fd.
