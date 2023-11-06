%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Problem Solving and Search                                %
% Ulle Endriss (ulle.endriss@uva.nl)                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tic-Tac-Toe: State-Space Representation                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% States are represented as terms of the form (Board,Player).
% The names of the two players are max and min. The following 
% predicate can be used to switch between players.

other(max, min).
other(min, max).

% The following predicate specifies the value of the game 
% associated with the given player winning that game.

value(max, +1).
value(min, -1).

% Every board configuration is represented as a list of three lists,
% such as [ [x,-,o], [-,-,o], [-,x,-] ]. Here - represents an empty
% cell. The following predicate specified the symbol associated with 
% each of the two players.

symbol(max, o).
symbol(min, x).
 
% Retrieving the initial state:

initial((Board,max)) :- 
  Board = [ [-,-,-], [-,-,-], [-,-,-] ].

% A move amounts to the player whose turn it is to place a symbol
% on one of the free cells of the board. It also involves a switch
% of the turn to the other player.

move((Board,Player), (NewBoard,OtherPlayer)) :-
  append(TopRows, [Row|BottomRows], Board),
  append(LeftCells, [-|RightCells], Row),
  symbol(Player, Symbol),
  append(LeftCells, [Symbol|RightCells], NewRow),
  append(TopRows, [NewRow|BottomRows], NewBoard),
  other(Player, OtherPlayer).

% A state is terminal if either one of the two players has a complete 
% "line" with his/her symbol (in which case the value of the game is 
% equal to the ideal value of that player) or if there are no more empty 
% cells left on the board (in which case the value is 0). Note that we 
% can never reach a board configuration in which both players have a line, 
% because the game would have terminated before. (In this counterfactual 
% situation, for our implementation the value returned would be +1, 
% because this is the value of the /first/ player.)

terminal((Board,_), Value) :-
  symbol(Player, Symbol), 
  line(Board, [Symbol,Symbol,Symbol]), !,
  value(Player, Value).

terminal((Board,_), 0) :- 
  \+ ( member(Row, Board), member(-, Row) ).

% Auxiliary predicate to return all 8 "lines" for a given configuration
% upon backtracking.
 
line([[A,B,C], [_,_,_], [_,_,_]], [A,B,C]).
line([[_,_,_], [A,B,C], [_,_,_]], [A,B,C]).
line([[_,_,_], [_,_,_], [A,B,C]], [A,B,C]).
line([[A,_,_], [B,_,_], [C,_,_]], [A,B,C]).
line([[_,A,_], [_,B,_], [_,C,_]], [A,B,C]).
line([[_,_,A], [_,_,B], [_,_,C]], [A,B,C]).
line([[A,_,_], [_,B,_], [_,_,C]], [A,B,C]).
line([[_,_,A], [_,B,_], [C,_,_]], [A,B,C]).

% Heuristic evaluation function that first counts 
% (a) how many lines are not yet blocked for Max by Min's symbol and
% (b) how many lines are not yet blocked for Min by Max's symbol.
% We then compute a heuristic evaluation by calculating the difference 
% between these two numbers and dividing by 8. 

estimate((Board,_), Value) :-
  symbol(max, MaxSym),
  symbol(min, MinSym),
  findall(L, (line(Board,L), \+ member(MinSym,L)), MaxLines),
  findall(L, (line(Board,L), \+ member(MaxSym,L)), MinLines),
  length(MaxLines, MaxChances),
  length(MinLines, MinChances),
  Value is (MaxChances - MinChances) / 8.

