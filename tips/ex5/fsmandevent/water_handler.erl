-module(water_handler).
-compile(export_all).

%% State = solid or liquid or gas
init(InitState) ->
  InitState.

terminate(State) ->
  State.

%% handle_event(Event, Data) => NewData
handle_event({boil, Tool}, liquid) ->
  io:format("boiling by ~p.~n", [Tool]),
  gas;


handle_event({freeze, Tool}, liquid) ->
  io:format("freeze by ~p.~n", [Tool]),
  solid;

handle_event({thaw, Tool}, solid) ->
  io:format("thaw by Tool~p.~n", [Tool]),
  liquid;

handle_event({cooling, Tool}, gas) ->
  io:format("cooling with Tool.~p.~n", [Tool]),
  water;

handle_event(get, State) ->
  io:format("current State is ~p~n", [State]),
  State.
