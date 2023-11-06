:- discontiguous move/2.
:- discontiguous moves/2.
:- discontiguous eval/2.
:- discontiguous terminal/2.

:- consult('tictactoe.pl').

:- dynamic counter/1.
 
init_counter :- 
    retractall(_),
    assert(counter(0)).

add_counter :-
    retract(counter(Rollout)),
    NewRollout is Rollout + 1,
    assert(counter(Rollout)).

get_counter :-
    counter(X).


eval(State, Value) :-
    terminal(State, Value), !, add_counter.

moves(State, NextStates) :-
    findall(NextState, move(State, NextState), NextStates).

eval((Board, max), Value) :-
    moves((Board, max), NextStates), % all potential next states
    maxeval(NextStates, _, Value). % max value from next states
    
eval((Board, min), Value) :- % Analogously for Mindy!
    moves((Board, min), NextStates),
    mineval(NextStates, _, Value).


% Base case: there is just one state. return it and its value
maxeval([State], State, Value) :- !, eval(State, Value).
% Otherwise, compute the value of the first state and
% the max-value state and its value for the tail,
% then choose the better state/value pair of the two.
maxeval([State1|States], MaxState, MaxValue):-
    eval(State1, Value1),
    maxeval(States, State, Value),
    choose(Value > Value1,
        (State, Value),
        (State1, Value1),
        (MaxState, MaxValue)).

mineval([State], State, Value) :- !, eval(State, Value).

mineval([State1|States], MinState, MinValue) :-
    eval(State1, Value1),
    mineval(States, State, Value),
    choose(Value < Value1,
        (State,Value),
        (State1,Value1),
        (MinState,MinValue)).

moves(State, NextStates) :-
    findall(NextState, move(State, NextState), NextStates).

choose(Condition, X, _, X) :- call(Condition), !.
choose(_, _, Y, Y).

minimax((Board, max), MaxState) :-
    moves((Board, max), NextStates),
    maxeval(NextStates, MaxState, _).

minimax((Board, min), MinState) :-
    moves((Board, min), NextStates),
    mineval(NextStates, MinState, _).

% minimax(([[x,o,o], [-,x,o], [-,-,-]],min), (Board,_) ).
% initial(State), time( minimax(State, NextState) ).
