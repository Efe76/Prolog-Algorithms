% Distances between cities
distance(amsterdam, utrecht, 44).
distance(amsterdam, rotterdam, 76).
distance(amsterdam, leeuwarden, 134).
distance(utrecht, rotterdam, 58).
distance(utrecht, haarlem, 40).
distance(utrecht, rotterdam, 60).
distance(utrecht, hertogenbosch, 60).
distance(utrecht, breda, 85).
distance(haarlem, rotterdam, 70).
distance(hertogenbosch, rotterdam, 85).
distance(hertogenbosch, breda, 45).
distance(leeuwarden, groningen, 95).
distance(leeuwarden, zwolle, 85).
distance(groningen, zwolle, 120).
distance(eindhoven, hertogenbosch, 34).
distance(breda, eindhoven, 64).
distance(zwolle, amsterdam, 105).
distance(maastricht, eindhoven, 87).

% Coordinates of cities (In KiloMeters)
coordinates(amsterdam, 5803.83 , 543.67).
coordinates(breda, 5733.44, 529.54).
coordinates(eindhoven, 5689.57, 606.75).
coordinates(groningen, 5902.80, 728.48).
coordinates(haarlem, 5807.77, 515.52).
coordinates(hertogenbosch, 5737.15, 586.36).
coordinates(leeuwarden, 5902.80, 662.51).
coordinates(maastricht, 5635.29, 632.24).
coordinates(rotterdam, 5755.93, 498.11).  
coordinates(utrecht, 5779.74, 568.28).    
coordinates(zwolle, 5826.54, 675.93). 


% Predicate, checks the distance between Current and Next city
% Move/3
move(CurrentCity, NextCity, Distance) :-
    distance(CurrentCity, NextCity, Distance).

move(CurrentCity, NextCity, Distance) :-
    distance(NextCity, CurrentCity, Distance).


% Estimates the straight line distance using Euclidean distance. Gives us the Estimate cost of a certain city.
% Estimate/2
estimate(CityStart, Distance) :-
  goal(CityLast),
  coordinates(CityStart, X1, Y1),
  coordinates(CityLast, X2, Y2), 
  euclidean_distance(X1, Y1, X2, Y2, Distance).

euclidean_distance(X1, Y1, X2, Y2, Distance) :-
  Distance is sqrt((X2 - X1)**2 + (Y2 - Y1)**2).


% Asserting goal/1 predicate dynamically, to be able to use the program with different target cities
:- dynamic goal/1.

% Route predicate to call AStar Algorithm. Will return multiple routes with corresponding distances
% Route/4
route(CityStart, CityLaatst, Route, Distance) :-
  retractall(goal(_)), 
  assert(goal(CityLaatst)), 
  solve_astar(CityStart, Route/Distance).


% the A* algorithm for heuristic-guided best-first search
solve_astar(Node, Path/Cost) :-
    estimate(Node, Estimate),
    astar([[Node]/0/Estimate], RevPath/Cost/_),
    reverse(RevPath, Path).
  
  astar(Paths, Path) :-
    get_best(Paths, Path),
    Path = [Node|_]/_/_,
    goal(Node).
  
  astar(Paths, SolutionPath) :-
    get_best(Paths, BestPath),
    select(BestPath, Paths, OtherPaths),
    expand_astar(BestPath, ExpPaths),
    append(OtherPaths, ExpPaths, NewPaths),
    astar(NewPaths, SolutionPath).
  
  get_best([Path], Path) :- !.
  
  get_best([Path1/Cost1/Est1,_/Cost2/Est2|Paths], BestPath) :-
    Cost1 + Est1 =< Cost2 + Est2, !,
    get_best([Path1/Cost1/Est1|Paths], BestPath).
  
  get_best([_|Paths], BestPath) :-
    get_best(Paths, BestPath).
  
  expand_astar(Path, ExpPaths) :-
    findall(NewPath, move_astar(Path,NewPath), ExpPaths).
  
  move_astar([Node|Path]/Cost/_, [NextNode,Node|Path]/NewCost/Estimate) :-
    move(Node, NextNode, StepCost),
    \+ member(NextNode, Path),
    NewCost is Cost + StepCost,
    estimate(NextNode, Estimate).
