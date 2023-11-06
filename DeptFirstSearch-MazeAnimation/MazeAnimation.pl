% Example output of 2 steps for a 5x5 grid, after final step "z" should be right bottom of the grid
% (It looks better in the terminal, just use query -> ?- solve_depthfirst_cyclefree(1/1, Path), animate(Path). )
% +---+---+---+---+---+
% | z |   |   |||||||||
% +---+---+---+---+---+
% |||||   |   |   |   |
% +---+---+---+---+---+
% |   |   |   |||||||||
% +---+---+---+---+---+
% |   |||||   |   |   |
% +---+---+---+---+---+
% |   |   |   |||||   |
% +---+---+---+---+---+


% +---+---+---+---+---+
% |   | z |   |||||||||
% +---+---+---+---+---+
% |||||   |   |   |   |
% +---+---+---+---+---+
% |   |   |   |||||||||
% +---+---+---+---+---+
% |   |||||   |   |   |
% +---+---+---+---+---+
% |   |   |   |||||   |
% +---+---+---+---+---+


% Grid of the maze, "w" = White and "b" = Black. White is an open space, Black is a wall/obstacle
% The grid can be changed to whatever u want, just make sure it is a square so 5/5, 6/6 etc.
% The animation work on "all" grids.
grid([ [w, w, w, b, b, w, w, w, w, b],
       [b, w, w, w, w, w, b, b, b, b],
       [w, w, w, b, b, w, b, w, w, w],
       [w, b, w, w, w, w, w, w, b, w],
       [w, w, w, b, w, w, b, w, b, w],
       [b, b, b, b, w, w, b, w, b, w],
       [w, b, w, w, w, w, b, b, b, b],
       [w, b, w, b, b, b, b, w, w, b],
       [w, b, w, w, w, w, b, w, w, b],
       [w, w, w, b, b, w, w, w, w, w] ]).

% Predicate, check if current tile X/Y is white.
% white/1
white(X/Y) :- 
    grid(Grid), 
    % nth1 is a built in predicate.
    nth1(Y, Grid, Rij), 
    nth1(X, Rij, w). 

% Five Predicates, move Horizontally/Vertically
% Move/2
move(X/Y, AlternPositions) :- 
    AltX is X - 1, % Left
    AltY is Y,
    white(AltX/AltY), 
    AlternPositions = AltX/AltY.

move(X/Y, AlternPositions) :-
    AltX is X + 1, % Right
    AltY is Y,
    white(AltX/AltY),
    AlternPositions = AltX/AltY.

move(X/Y, AlternPositions) :-
    AltX is X,
    AltY is Y - 1, % Up
    white(AltX/AltY),
    AlternPositions = AltX/AltY.

move(X/Y, AlternPositions) :-
    AltX is X,
    AltY is Y + 1, % Down
    white(AltX/AltY),
    AlternPositions = AltX/AltY.

% Get length of Grid.
gridsize(Size) :-
  grid(Grid),
  length(Grid, Size).

% Goal is at right bottom, so always 5/5, 6/6, 7/7 etc. unless "dumb" enough to make grid not a square.
goal(Size/Size) :-
  gridsize(Size).

% Cycle free DFS,

% - Bounded DFS also works just as good, since its a variant of CycleFree DFS. Annoying part is you have to define a value to the Bound variable.
% if the value of Bound variable is too low it wont work. But since these mazes are quite basic, i used the normal Cycle Free DFS.

% - Plain DFS fell into a infinite loop since it doesnt remember visited nodes, causing it to go to node 2 back to node 1 back to node 2.

solve_depthfirst_cyclefree(Node, Path) :-
    depthfirst_cyclefree([Node], Node, RevPath),
    reverse(RevPath, Path). % Reverse paths to get the form (a, b, c) instead of (c, b, a). 
  
depthfirst_cyclefree(Visited, Node, Visited) :-
    goal(Node).
  
depthfirst_cyclefree(Visited, Node, Path) :- 
    move_cyclefree(Visited, Node, NextNode),
    depthfirst_cyclefree([NextNode|Visited], NextNode, Path).

move_cyclefree(Visited, Node, NextNode) :-
    move(Node, NextNode),
    \+ member(NextNode, Visited). % Check if the node about to move on not already visited. Protects us from infintie looop


% ?- solve_depthfirst_cyclefree(1/1, Path), animate(Path).
% The following "screenshot" shows the situation at the end of the animation:
% +---+---+---+---+---+---+---+---+---+---+
% |   |   |   |||||||||   |   |   |   |||||
% +---+---+---+---+---+---+---+---+---+---+
% |||||   |   |   |   |   |||||||||||||||||
% +---+---+---+---+---+---+---+---+---+---+
% |   |   |   |||||||||   |||||   |   |   |
% +---+---+---+---+---+---+---+---+---+---+
% |   |||||   |   |   |   |   |   |||||   |
% +---+---+---+---+---+---+---+---+---+---+
% |   |   |   |||||   |   |||||   |||||   |
% +---+---+---+---+---+---+---+---+---+---+
% |||||||||||||||||   |   |||||   |||||   |
% +---+---+---+---+---+---+---+---+---+---+
% |   |||||   |   |   |   |||||||||||||||||
% +---+---+---+---+---+---+---+---+---+---+
% |   |||||   |||||||||||||||||   |   |||||
% +---+---+---+---+---+---+---+---+---+---+
% |   |||||   |   |   |   |||||   |   |||||
% +---+---+---+---+---+---+---+---+---+---+
% |   |   |   |||||||||   |   |   |   | z |
% +---+---+---+---+---+---+---+---+---+---+

% Go up and make sure grid, has has a reachable goal
animate([H|T]) :-
    flushscreen, % Clean terminal
    show(H, z),
    sleep(0.3), % Pause 0.3 secs
    animate(T).  
  

% Jump animation if goal reached
animate([]) :-
    gridsize(N),
    Celebrate = (flushscreen, show(N/N, 'Z'), sleep(0.1), 
                 flushscreen, show(N/N, z), sleep(0.1) ),
    multicall(8, Celebrate),
    sleep(1).
  

% Grid printed with my robot on X/y Postion
% Sym = Symbol of the character.

show(X/Y, Sym) :-
    gridsize(N),
    multicall(N, write('+---')), write('+'), nl, 
    show(X/Y, 1/1, Sym).
  

show(X/Y, N/N, Sym) :- 
    gridsize(N), !,
    showcell(X/Y, N/N, Sym), 
    write('|'), nl,
    multicall(N, write('+---')), write('+'), nl, nl, nl.
  
show(X/Y, N/CY, Sym) :-
    gridsize(N), !,
    showcell(X/Y, N/CY, Sym), 
    write('|'), nl,
    multicall(N, write('+---')), write('+'), nl, 
    NewCY is CY + 1,
    show(X/Y, 1/NewCY, Sym).
  
show(X/Y, CX/CY, Sym) :-
    showcell(X/Y, CX/CY, Sym), 
    NewCX is CX + 1,
    show(X/Y, NewCX/CY, Sym).


showcell(Pos, Pos, Sym) :- !,
    write('| '),
    write(Sym),
    write(' ').
  
showcell(_, Pos, _) :-
    white(Pos), !,
    write('|   ').
  
%Obstacle if black tile
showcell(_, _, _) :-
    write('||||').
  
% Refresh terminal
flushscreen :- % Cleanup Terminal for next animation of maze, unless goal already reached
    multicall(30, nl).

  
multicall(0, _).
  
multicall(N, Goal) :-
    N > 0,
    call(Goal),
    N1 is N - 1,
    multicall(N1, Goal).

 % Use following query, 1/1 can be changed to whatever ur starting point desire is
% ?- solve_depthfirst_cyclefree(1/1, Path), animate(Path).
