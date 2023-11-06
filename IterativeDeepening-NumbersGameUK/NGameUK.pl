% Predicate move/2, to perform operations on elements in "Terms"
move(Target:Terms, Target:[Result|Terms2]) :- 

    select(X, Terms, Terms1), % Select an element X from "Terms", Resulting in Terms1 without element X
    select(Y, Terms1, Terms2), % Select another element Y out of "Terms1". Resulting in "Terms2" without X and Y.

    operation(X, Y, Result). % Now we use a operations on these two elements.


% We dont want to use "is" as this will calculate it instantly, we want it to stay X + Y, X * Y etc.
% Operations/3
operation(X, Y, Result) :-
  Result = X + Y. % Addition
  
operation(X, Y, Result) :-
  Result = X - Y.% Subtraction
  
operation(X, Y, Result) :-
  Result = X * Y. % Multipication
  
operation(X, Y, Result) :-
  Y =\= 0, % Y cannot be 0, to avoid division by zero
  X =\= 0,

  TempResult is X / Y, % Check if Result is not a float
  integer(TempResult), % Check if Result is not a float

  Result = X / Y. % if not a float than valid Division


% goal/1 predicate succeeds if the 'Target' is equal to Result
goal(Target:Terms) :- 
    select(Result, Terms, _), 
    Target is Result. % With "is" we calculate the Result which was stored as "100+75 etc." for example, then check if equal to Target.


% The 'solve/3' predicate attempts to solve the problem by finding a solution using all or some of the numbers in "Terms"
solve(Target, Terms, Head) :- 

    solve_iterative_deepening(Target:Terms, Solution), 
  
    append(_, [Target:[Head|_]], Solution). % We use this to refine "Solution" in the form we want which is: 50*6-75/25 

solve_iterative_deepening(Node, Path) :-
    path(Node, GoalNode, RevPath),
    goal(GoalNode),
    reverse(RevPath, Path).
  
  path(Node, Node, [Node]).
  
  path(FirstNode, LastNode, [LastNode|Path]) :-
    path(FirstNode, PenultimateNode, Path),
    move_cyclefree(Path, PenultimateNode, LastNode).


% Auxiliary predicate: wrapper around move/2.
move_cyclefree(Visited, Node, NextNode) :-
    move(Node, NextNode),
    \+ member(NextNode, Visited).


  %% Query example %%
% ?- solve(297, [100,75,50,25,6,4], Solution).
% Solution = 50*6-75/25 






%----------------------------------------------------------------------------------------------------------------------------------------------------------
% TESTING DIFFERENT ALGORTIHMS SUCH AS BFS, DFS (Plain, Cyclefree and Bounded)
% DFS Cycle% WORKS
solve1(Target, Terms, Head) :-
  solve_depthfirst_cyclefree(Target:Terms, Solution),
  append(_, [Target:[Head|_]], Solution).

% solve1(297, [100,75,50,25,6,4], Solution).
% Solution = 50*6-((100+75)/25-4) .


% BFS % DOES NOT WORK
solve2(Target, Terms, Head) :-
  solve_breadthfirst(Target:Terms, Solution),
  append(_, [Target:[Head|_]], Solution).

% solve2(297, [100,75,50,25,6,4], Solution).
% ERROR: Stack limit (1.0Gb) exceeded
% ERROR:   Stack sizes: local: 1Kb, global: 0.9Gb, trail: 1Kb
% Gets stuck in a loop, causing it to exceed the stack limit


% DFS Plain % WORKS
solve3(Target, Terms, Head) :-
  solve_depthfirst(Target:Terms, Solution),
  append(_, [Target:[Head|_]], Solution).

% solve3(297, [100,75,50,25,6,4], Solution).
% Solution = 50*6-((100+75)/25-4) .


% Simple depth-first search:

solve_depthfirst(Node, [Node|Path]) :-
  depthfirst(Node, Path).

depthfirst(Node, []) :-
  goal(Node).

depthfirst(Node, [NextNode|Path]) :-
  move(Node, NextNode),
  depthfirst(NextNode, Path).

% Cycle-free depth-first search:

solve_depthfirst_cyclefree(Node, Path) :-
  depthfirst_cyclefree([Node], Node, RevPath),
  reverse(RevPath, Path).

depthfirst_cyclefree(Visited, Node, Visited) :-
  goal(Node).

depthfirst_cyclefree(Visited, Node, Path) :-
  move_cyclefree(Visited, Node, NextNode),
  depthfirst_cyclefree([NextNode|Visited], NextNode, Path).

% Depth-bounded depth-first search:

solve_depthfirst_bound(Bound, Node, Path) :-
  depthfirst_bound(Bound, [Node], Node, RevPath),
  reverse(RevPath, Path).

depthfirst_bound(_, Visited, Node, Visited) :-
  goal(Node).

depthfirst_bound(Bound, Visited, Node, Path) :-
  Bound > 0,
  move_cyclefree(Visited, Node, NextNode),
  NewBound is Bound - 1,
  depthfirst_bound(NewBound, [NextNode|Visited], NextNode, Path).

% Breadth-first search:

solve_breadthfirst(Node, Path) :-
  breadthfirst([[Node]], RevPath),
  reverse(RevPath, Path).

breadthfirst([[Node|Path]|_], [Node|Path]) :-
  goal(Node).

breadthfirst([Path|Paths], SolutionPath) :-
  expand_breadthfirst(Path, ExpPaths),
  append(Paths, ExpPaths, NewPaths),
  breadthfirst(NewPaths, SolutionPath).

expand_breadthfirst([Node|Path], ExpPaths) :-
  findall([NewNode,Node|Path], move_cyclefree(Path,Node,NewNode), ExpPaths).
