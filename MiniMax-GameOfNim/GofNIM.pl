% Board configurations for the Game of Nim can be represented
% as lists of numbers, each representing the size of a heap.
% States are terms of the form (Board,Player), with Board
% being such a board configuration and Player being either
% the atom max or the atom min.

% The initial state for the game with three heaps of sizes
% three, four, and five, respectively, thus is represented
% by the term ([3,4,5],max), given that Max is the player
% whose turn it is in the initial state.

example(([3,4,5],max)).

example((Board, max)) :-
    Board = [ 3, 4, 5 ].

% The predicate other/2 can be used to switch between players.

other(max, min).
other(min, max).

% The predicate value/2 specifies the value of the game
% associated with the given player winning that game.

value(max, +1).
value(min, -1).

% We have reached a terminal state when all heaps have size
% zero. In that case the player whose turn it would be next is
% the winner, because this means that the other player must
% have taken the last match.
 terminal(([], _), _).

 % If player is max we check the state with this predicate
 terminal((Board,max), Result) :- 
 
   select(0, Board, NewBoard),
   terminal((NewBoard, max), Result),
   Result = +1, !.
 
  % If player min we check the state with this predicate
 terminal((Board, min), Result) :- 
 
   select(0, Board, NewBoard),
   terminal((NewBoard, min), Result),
   Result = -1, !.



% Between/3 is a prebuilt predicate. Giving us all numbers between 0 and "n - 1 ".
% In ascending form. The reason for ascending is because most efficient move for
% both max and min is a low number (Near the 0, or 0 itself) this way we dont run
% out of stack limit
lower(Result, Number) :- 

    NewNumber is Number - 1,
    between(0, NewNumber, Result). 

% Choose a heap, use lower to give it a new value, switch player
move((Board, Player), ([Result|NewBoard], OtherPlayer)) :-

  select(Number, Board, NewBoard),
  lower(Result, Number),
  other(Player, OtherPlayer).


% The following query shows that for an initial board [3,5,7],
% the optimal move for Max would be to remove a single match
% from the smallest heap:

% ?- time(alphabeta(([3,5,7],max), MaxState)).
% % 7,682,585 inferences, 0.622 CPU in 0.671 seconds (93% CPU, 12359848 Lips)
% MaxState = ([2, 5, 7], min) .
% Yes


% Given a state, the predicate rollout/3 generates a complete
% rollout from that state and then returns the terminal state
% and the value of the terminal state in that rollout.
% It works as follows: If the given state is already terminal,
% stop and return the state and its value (base case).
% Otherwise, use move/2 to generate a follow-up state and then
% recursively apply rollout/3 to that new state.

rollout(State, Result, Value) :-
    terminal(State, Value),
    !,
    Result = State.
  
  rollout(State, Result, Value) :-
    move(State, NextState),
    rollout(NextState, Result, Value).

% The predicate count/2 can be used to count the number of ways 
% in which a given goal can succeed. It is implemented by applying 
% findall/3 to the given goal. The "pattern" we collect for every 
% answer is simply the number 1 (any other term would do just as 
% well). In the end, we measure the length of the list of 1s we
% collected in this manner.  
count(Goal, Games) :- 
    findall(1, Goal, Goals), 
    length(Goals, Games).


% (a)
% ?- Board = [1,1,1,1,1], Goal = rollout((Board,max), Result, Value), count(Goal, Number).
% Board = [1, 1, 1, 1, 1],
% Goal = rollout(([1, 1, 1, 1, 1], max), Result, Value),
% Number = 120.

% (b)
% ?- Board = [5], Goal = rollout((Board,max), Result, Value), count(Goal, Number).
% Board = [5],
% Goal = rollout(([5], max), Result, Value),
% Number = 16.

% (c)
% ?- Board = [2,2,2,2,2], Goal = rollout((Board,max), Result, Value), count(Goal, Number).
% Board = [2, 2, 2, 2, 2],
% Goal = rollout(([2, 2, 2, 2, 2], max), Result, Value),
% Number = 291720.

% (d)
% ?- Board = [5,5], Goal = rollout((Board,max), Result, Value), count(Goal, Number).
% Board = [5, 5],
% Goal = rollout(([5, 5], max), Result, Value),
% Number = 6802.

% Return all follow-up states for a given state:

moves(State, NextStates) :-
  findall(NextState, move(State, NextState), NextStates).

% Choose whether to bind the variable provided as the fourth
% argument to either the term given as the second argument
% (if the goal provided as the first argument succeeds) or
% the term given as the third argument argument (otherwise):

choose(Condition, X, _, X) :- call(Condition), !.
choose(_, _, Y, Y).

% Given numbers X, A, and B with A  =< B, find the number
% closest to X within the interval [A, B]:

round(X, A, _, A) :- X < A, !.
round(X, _, B, B) :- X > B, !.
round(X, _, _, X).

% Predicate alphabeta(+State, -BestNextState)

alphabeta((Board,max), MaxState) :-
  moves((Board,max), NextStates),
  maxeval(NextStates, -1, +1, MaxState, _).

alphabeta((Board,min), MinState) :-
  moves((Board,min), NextStates),
  mineval(NextStates, -1, +1, MinState, _).

% Predicate eval(+State, +Alpha, +Beta, -Value)

eval(_, Value, Value, Value) :- !.

eval(State, Alpha, Beta, RoundedValue) :-
  terminal(State, Value), !,
  round(Value, Alpha, Beta, RoundedValue).

eval((Board,max), Alpha, Beta, Value) :-
  moves((Board,max), NextStates),
  maxeval(NextStates, Alpha, Beta, _, Value).

eval((Board,min), Alpha, Beta, Value) :-
  moves((Board,min), NextStates),
  mineval(NextStates, Alpha, Beta, _, Value).

% Predicate maxeval(+States, +Alpha, +Beta, -MaxState, -MaxValue)

maxeval([State], Alpha, Beta, State, Value) :- !,
  eval(State, Alpha, Beta, Value).

maxeval([State1|States], Alpha, Beta, MaxState, MaxValue) :-
  eval(State1, Alpha, Beta, Value1),
  maxeval(States, Value1, Beta, State, Value),
  choose(Value > Value1, (State,Value), (State1,Value1), (MaxState,MaxValue)).

% Predicate mineval(+States, +Alpha, +Beta, -MinState, -MinValue)

mineval([State], Alpha, Beta, State, Value) :- !,
  eval(State, Alpha, Beta, Value).

mineval([State1|States], Alpha, Beta, MinState, MinValue) :-
  eval(State1, Alpha, Beta, Value1),
  mineval(States, Alpha, Value1, State, Value),
  choose(Value < Value1, (State,Value), (State1,Value1), (MinState,MinValue)).