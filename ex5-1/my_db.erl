-module(my_db).
-export([start/0, stop/0, write/2, delete/1, read/1, match/1]).
-export([init/0, loop/1, deleteKey/2, find/2]). 
%%-compile(export_all).
-record(data, {key, value}).
-define(TIMEOUT, 2000).

%% public API
start() ->
  {Pid, Ref} = spawn_monitor(?MODULE, init, []),
  register(db, Pid),
  Ref.

stop() ->
  db ! stop,
  receive
    {'DOWN', Ref, process, _Pid, Reason} ->
      io:format("server stopped, Ref : ~p, Reason : ~p ~n", [Ref, Reason])
  end.

write(Key, Value) ->
  async({write, Key, Value}).

delete(Key) ->
  async({delete, Key}).

read(Key) ->
  sync({read, Key},fun id/1).
        
match(Value) ->
  sync({match, Value},fun id/1).


%% private API
async(Msg) -> db ! Msg, ok. 
sync(Msg, Callback) ->
  Ref = make_ref(),
  db ! {self(), Ref, Msg},
  receive
    {Ref, Reply} -> Callback(Reply)
  after ?TIMEOUT -> io:format("error ~p~n", [Msg]), erlang:error(timeout)
  end.

%%% private helper function
id(Msg) -> Msg.


%% for server
init() ->
  InitData = [],
  loop(InitData).

loop(Data) ->
  receive
    stop ->
      io:format("shutdown!~nNON-preserved data : ~p~n", [Data]),
      exit(shutdown);
    {write, Key, Value} ->
      NewData = [#data{key=Key, value=Value}|Data],
      io:format("server.write NewData : ~p~n", [NewData]),
      loop(NewData);
    {delete, Key} ->
      %%NewData = [X || X <- Data, X#data.key =/= Key],
      NewData = deleteKey(Data, Key),
      io:format("server.write NewData : ~p~n", [NewData]),
      loop(NewData);
    {From, Ref, {read, Key}} ->
      case find(Data, Key) of
        {true, Value} -> 
          From ! {Ref, {ok, Value}},
          io:format("find key! : ~p~n", [Value]);
        false ->
          From ! {Ref, {error, instance}},
          io:format("error~n")
      end,
      loop(Data);
    {From, Ref, {match, Value}} ->
      Keys = [X#data.key || X <- Data, X#data.value =:= Value],
      From ! {Ref, Keys},
      io:format("loop => matches ~p ~n", [Keys]),
      loop(Data)
  end.


%%% private for server

deleteKey([], _) -> [];
deleteKey([H = #data{key=HKey}|T], Key) ->
  case HKey =:= Key of
    true -> T;
    false -> [H|deleteKey(T, Key)]
  end.

find([], _) -> false;
find([#data{key=HKey, value=HValue}|T], Key) ->
  case HKey =:= Key of
    true -> {true, HValue};
    false -> find(T, Key)
  end.

