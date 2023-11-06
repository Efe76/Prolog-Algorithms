%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Problem Solving and Search                                %
% Ulle Endriss (ulle.endriss@uva.nl)                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Interactive Game Console for Tic-Tac-Toe                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Use this query to play Tic-Tac-Toe against the computer:
% ?- play.

% Make available both the minimax algorithm and the state-space 
% representation of Tic-Tac-Toe:

:- consult('minimax.pl'),
   consult('tictactoe.pl').

% Specify the adversarial search algorithm to be used (you can 
% change this line later on, once you have learned about more 
% advanced algorithms):

algorithm(State, NewState) :- minimax(State, NewState).

% Board configurations will be printed as in this example:

%   1 | o | 3  
%  ---+---+---
%   x | 5 | 6  
%  ---+---+---
%   7 | 8 | 9 

% Initialise the state and ask the user whether she wants to play noughts 
% or crosses. Then call play/2 with those parameters.

play :-
  initial(State),
  nl, write('Interactive Tic-Tac-Toe ...'), nl,
  write('Do you want to play noughts (o) or crosses (x)? >> '),
  getPlayerChoice(Choice, [o,x]),
  symbol(UserPlayer, Choice),
  play(State, UserPlayer).

% Predicate that takes the description of the current state and the 
% name of the player representing the user and reacts appropriately: 
% (a) print final message in case the current state is terminal, 
% (b) compute and execute the next move for the computer in case 
% it is the computer's turn, or 
% (c) ask the user for her next move and execute it in case it is her turn.

play((Board,Player), UserPlayer) :-
  terminal((Board,Player), Outcome), !,
  flush(40),
  draw(Board), nl,
  finalMessage(Outcome, UserPlayer, Message),
  write(Message), nl.

play((Board,ComputerPlayer), UserPlayer) :-
  flush(40),
  other(ComputerPlayer, UserPlayer),
  draw(Board), nl, 
  write('Thinking about my next move ...'), nl, sleep(1),
  algorithm((Board,ComputerPlayer), NewState), 
  play(NewState, UserPlayer).

play((Board,UserPlayer), UserPlayer) :-
  symbol(UserPlayer, UserSymbol),
  options(Board, Options),
  atoms_numbers(CharOptions, Options),
  flush(40),
  draw(Board), nl, nl,
  write('Choose where to put your next '), write(UserSymbol),
  write(' (options: '), writeList(Options), write(')! >> '),
  getPlayerChoice(CharChoice, CharOptions),
  atom_number(CharChoice, Choice),
  insert(Board, Choice, UserSymbol, NewBoard),
  other(UserPlayer, ComputerPlayer),
  play((NewBoard,ComputerPlayer), UserPlayer).

% Wait until the user presses a key corresponding to one of the characters
%  included in the list AllowedChoices and then bind that character to the 
% variable Choice. Example: Try the query getPlayerChoice(X, [a,b,c]).

getPlayerChoice(Choice, AllowedChoices) :-
  get_single_char(Code), 
  char_code(Choice, Code),
  member(Choice, AllowedChoices), !.

getPlayerChoice(Choice, AllowedChoices) :-
  getPlayerChoice(Choice, AllowedChoices).
  
% Find the numbers indexing the fields on the board that are still empty 
% (i.e., that are currently populated by a -):

options(Board, Options) :-
  flatten(Board, List),
  findall(N, nth1(N, List, -), Options).

% Insert a given symbol at a given position on the board:

insert(Board, Position, Symbol, NewBoard) :- 
  flatten(Board, List),
  N is Position - 1,
  length(Prefix, N),
  append(Prefix, [_|Postfix], List),
  append(Prefix, [Symbol|Postfix], NewList),
  NewList = [A,B,C,D,E,F,G,H,I],
  NewBoard = [[A,B,C],[D,E,F],[G,H,I]].

% Draw the given board configuration on the screen:

draw([X,Y,Z]) :-
  indent(10), drawRow(0, X), 
  indent(10), write('---+---+---'), nl,
  indent(10), drawRow(3, Y),
  indent(10), write('---+---+---'), nl,
  indent(10), drawRow(6, Z).

drawRow(N, [A,B,C]) :-
  write(' '), 
  getSymbol(N+1, A, AS), write(AS), write(' | '),
  getSymbol(N+2, B, BS), write(BS), write(' | '),
  getSymbol(N+3, C, CS), write(CS), nl.

getSymbol(N, -, S) :- !, S is N.
getSymbol(_, S, S).

% Retrieve the final message for a game. This message depends on both the 
% value of the terminal state reached and on the identity of the player 
% representing the user.

finalMessage(0, _, 'It\'s a draw!') :- !.

finalMessage(Value, UserPlayer, 'You win!') :-
  value(UserPlayer, Value), !.

finalMessage(_, _, 'You lose!').

% Print the elements of a list (with commas, but without square brackets):

writeList([]).
writeList([X]) :- !, write(X).
writeList([H|T]) :- write(H), write(', '), writeList(T).

% Print a given number of blanks (' '):

indent(0) :- !.
indent(N) :- write(' '), N1 is N-1, indent(N1).

% Flush the screen by printing a given number of empty lines:

flush(0) :- !.
flush(N) :- nl, N1 is N-1, flush(N1).

% Apply atom_number/2 to the elements of a list. This is useful to translate 
% back and forth between (one-digit) numbers and the corresponding characters.

atoms_numbers([], []).
atoms_numbers([A|As], [N|Ns]) :- atom_number(A, N), atoms_numbers(As, Ns).
