-module(water_fsm).
%%-compile(export_all).
-export([start/0, drink/0]).
-export([get_state/0, boil/1, freeze/1, thaw/1, cooling/1]).
start() ->
  register(water, spawn(fun() -> init() end)).
drink() ->
  exit(whereis(water), drink).


init() ->
  liquid().

%% sync
get_state() ->
  Ref = make_ref(),
  water ! {self(), Ref, get},
  receive
    {Ref, {state, State}} ->
      State
  end.

%% event
boil(Tool) ->
  water ! {boil, Tool}.

freeze(Tool) ->
  water ! {freeze, Tool}.

thaw(HowTo) ->
  water ! {freeze, HowTo}.

cooling(Tool) ->
  water ! {cooling, Tool}.

%% state
liquid() ->
  receive
    {boil, Tool} ->
      io:format("boiled with ~p~n", [Tool]),
      gas();
    {freeze, Tool} ->
      io:format("freezed with ~p~n", [Tool]),
      solid();
    {From, Ref, get} ->
      From ! {Ref, {state, liquid}},
      liquid()
  end.

solid() ->
  receive
    {thaw, Tool} ->
      io:format("thawed with ~p~n", [Tool]),
      liquid();
    {From, Ref, get} ->
      From ! {Ref, {state, solid}},
      solid()
  end.

gas() ->
  receive
    {cooling, Tool} ->
      io:format("cooling by Tool ~p~n", [Tool]),
      liquid();
    {From, Ref, get} ->
      From ! {Ref, {state, gas}},
      gas()
  end.
