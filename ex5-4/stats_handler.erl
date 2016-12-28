-module(stats_handler).
-compile(export_all).

init(InitData) -> {[], 0}.

terminate(Data) -> {count, Data}.

%% handle_event/2(Event, Data) => NewData.
handle_event({Type, _, Description}, Data = {List, Count}) ->
  NewData = case lists:member({Type, Description}, List) of
    false ->
      {[{Type, Description}|List], Count+1};
    true ->
      Data
  end,
  io:format("stat : ~p~n",[NewData]),
  NewData;

%% for event_manager:get_data(Name, Handler).
handle_event(get, {_, Count}) ->
  Count;

%% otherwise  
handle_event(_, Data) -> Data. 
