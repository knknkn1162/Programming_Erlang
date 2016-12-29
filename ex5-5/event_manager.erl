-module(event_manager).
-compile(export_all).

start(Name, HandlerList) ->
  register(Name, spawn(?MODULE, init, [HandlerList])), ok.


init(HandlerList) ->
  loop(initialize(HandlerList)).

initialize([]) -> [];
initialize([{Handler, InitData}|Rest]) ->
  [{Handler, Handler:init(InitData)}|initialize(Rest)].


stop(Name) ->
  Name ! {stop, self()},
  receive
    {reply, Reply} ->
      Reply
  end.

terminate([]) -> 
  [];
terminate([{Handler, Data}|Rest]) ->
  [{Handler, Handler:terminate(Data)}|terminate(Rest)].


%% event action
add_handler(Name, Handler, InitData) ->
  call(Name, {add_handler, Handler, InitData}).

delete_handler(Name, Handler) ->
  call(Name, {delete_handler, Handler}).

get_data(Name, Handler) ->
  call(Name, {get_data, Handler}).

send_event(Name, Event) ->
  call(Name, {send_event, Event}).

%%% answer for ex5-3
%%% Note) the problem requires for connecting from OldHanlder:terminate/1 to NewHandler:init/1.
%%% but I couldn't understand how I can change from io_handler to log_hanldler inspiteof the definitely different return value({count, Count} msg and ok msg are pretty diffrerent!!). So, in args3, I took the format : State = {NewHandler, InitData}.
swap_handlers(Name, OldHandler, NewState) ->
  call(Name, {swap_handlers, OldHandler, NewState}).  


%% helper
%%% sync
call(Name, Msg) ->
  Name ! {request, self(), Msg},
  receive
    {reply, Reply} ->
      Reply
  end.
%%% async
reply(To, Msg) ->
  To ! {reply, Msg}.

  
%%% State = [{Handler, Data}]
loop(State) ->
  receive
    %% receive sync Msg
    {request, From, Msg} ->
      {Reply, NewState} = handle_msg(Msg, State),
      reply(From, Reply),
      loop(NewState);
    %%stop function 
    {stop, From} ->
      reply(From, terminate(State))
  end.


%%% Note) You have to be ready for the function, Handler:init/1, terminate/1, handle_event/2
%%% handle_msg(Msg, State) => {Reply, NewState}
%%%% from add_handler/3
handle_msg({add_handler, Handler, InitData}, LoopData) ->
  {ok, [{Handler, Handler:init(InitData)}|LoopData]};
%%%% from delete/handler/2
handle_msg({delete_handler, Handler}, LoopData) ->
  case lists:keysearch(Handler, 1, LoopData) of
    false ->
      {{error, instance}, LoopData};
    {value, {Handler, Data}} ->
      Reply = {data, Handler:terminate(Data)},
      NewLoopData = lists:keydelete(Handler, 1, LoopData),
      {Reply, NewLoopData}
  end;
%% fix for ex5-4.
%%%% from get_data/2
handle_msg({get_data, Handler}, LoopData) ->
  case lists:keysearch(Handler, 1, LoopData) of
    false -> 
      {{error, instance}, LoopData};
    {value, {Handler, Data}} ->
      {{data, Handler:handle_event(get, Data)}, LoopData}
  end;
%%%% from send_event/2
handle_msg({send_event, Event}, LoopData) ->
  {ok, event(Event, LoopData)};
%%% from swap_handlers/3
handle_msg({swap_handlers, OldHandler, {NewHandler, InitData}}, LoopData) ->
  case lists:keysearch(OldHandler, 1, LoopData) of
    false ->
      {{error, instance}, LoopData};
    {value, {OldHandler, Data}} ->
      OldHandler:terminate(Data),
      NewLoopData = lists:keydelete(OldHandler, 1, LoopData),
      {ok, [{NewHandler, NewHandler:init(InitData)}|NewLoopData]}
  end.

%%% from handle_msg : event(Event, LoopData) => [{Handler, Result}]
event(_Event, []) -> [];
event(Event, [{Handler, Data}|Rest]) ->
  [{Handler, Handler:handle_event(Event, Data)}|event(Event, Rest)].
