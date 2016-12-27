-module(frequency).
-compile(export_all).
-define(TIMEOUT, 5000).
-record(cover, {freq, client}).



%% client API
start() ->
  register(frequency, spawn(frequency, init, [])).

init() ->
  Frequencies = get_frequencies(),
  loop(Frequencies).


stop() -> 
  Ref = monitor(process, frequency),
  async(stop),
  receive
    {'DOWN', Ref, process, _Pid, shutdown} ->
      io:format("server stop!, Ref : ~p, Pid : ~p ~n", [Ref, _Pid])
  after ?TIMEOUT -> io:format("error stop~n"), erlang:error(timeout)
  end.


allocate() -> sync(allocate).
deallocate(Freq) -> sync({deallocate, Freq}).

%%  helper
sync(Msg) ->
  Ref = make_ref(),
  frequency ! {self(), Ref, Msg},
  receive
    {Ref, Reply} -> Reply
  end.

async(Msg) ->
  frequency ! Msg.

%% for server
loop(Frequencies) ->
  receive
    %% synchronous
    {Pid, Ref, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      io:format("NewFreq : ~p, Reply : ~p~n", [NewFrequencies, Reply]),
      Pid ! {Ref, Reply},
      loop(NewFrequencies);
    {Pid, Ref, {deallocate, Freq}} ->
      {NewFrequencies, Reply} = deallocate(Frequencies, Freq, Pid),
      io:format("NewFreq : ~p, Reply : ~p~n", [NewFrequencies, Reply]),
      Pid ! {Ref, Reply},
      loop(NewFrequencies);
    %% async
    stop ->
      exit(shutdown)
  end.

allocate(Frequencies = {[], _}, _) ->
  {Frequencies, {error, no_frequency}};
allocate({[H|T], Cover}, Pid) ->
  NewCover = [#cover{freq=H, client=Pid}|Cover],
  {{T, NewCover}, ok}.


deallocate(Frequencies = {Freq, Cover}, RequestFreq, Pid) ->
  NewCover  = lists:delete(#cover{freq=RequestFreq, client=Pid}, Cover),
  case NewCover =:= Cover of 
    false ->
      {{[RequestFreq|Freq], NewCover}, ok};
    true ->
      {Frequencies, nothing_new}
  end.

%% Hard Coded
%% Note) Lists are Desplaying as character, [10] => "\n", [11] => "\v", [12] ="\f", [13] = "\r", 
%% so you don't have to be afraid of getting strange lists (e.g. [10,11,12,13] => ["\n", "\v", "\f", "\r"]).
%% that's actually allocatable frequency lists!!
get_frequencies() ->
  {lists:seq(10,15), []}.


