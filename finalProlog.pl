:- dynamic([
  map_size/1,	
  room/2,		 
  wumpus/1,		
  noPit/1,		
  noWumpus/1,		
  maybeVisitLater/2,	
  wumpusPath/1
   ]).

%% The starting point for execution
start:-
  % make sure to clear any previous facts stored
 
 retractall(wumpus(_)),
  retractall(noPit(_)),
  retractall(noWumpus(_)),
  retractall(maybeVisitLater(_,_)),
  retractall(wumpusPath(_)),
  % initializations
  init_cave,
  init_agent,
  init_wumpus,

  % agent starts searching from the roomn [1, 1]
  explore([1, 1], []),

  % visit stored paths if any
  maybeVisitLater(OldCell, LeadingPath),
  retract(maybeVisitLater(OldCell, _)),
  explore(OldCell, LeadingPath).

% Initialize the cave with Pits and Gold
init_cave:-
  retractall(map_size(_)),
  assert(map_size([4, 4])),	

  retractall(room(_, _)),
  assert(room(gold, [3, 2])),		

  % rooms of pits
  assert(room(pit, [1, 4])),
  assert(room(pit, [3, 3])),
  assert(room(pit, [4, 3])),
  assert(noPit([1, 1])). % No pit in room [1, 1].

%  Initialize the position of the Agent
init_agent:-
  assert(room(agent, [1, 1])).

% Initialize the position of the Wumpus
init_wumpus:-
  assert(room(wumpus, [4, 1])),
  assert(noWumpus([1, 1])).


%% DEFINING THE PERCEPTORS
% Helper predicate to check if cell room given is valid in the board.
valid_room([X, Y]):- X>0, Y>0, map_size([P, Q]), X@=<P, Y@=<Q.

% Generate adjacent rooms of a given room.
adjacent([X, Y], Z):- L is X-1, valid_room([L, Y]), Z=[L, Y].
adjacent([X, Y], Z):- R is X+1, valid_room([R, Y]), Z=[R, Y].
adjacent([X, Y], Z):- A is Y+1, valid_room([X, A]), Z=[X, A].
adjacent([X, Y], Z):- B is Y-1, valid_room([X, B]), Z=[X, B].

% A room is smelly if a cell with Wumpus is adjacent to it. There has to be at least one wumpus first.
stenchy([X, Y]):-
  room(wumpus, Z), \+ noWumpus(Z),
  adjacent([X, Y], Z).

% A room is breezy if a cell adjacent to it contains pit.
breezy([X, Y]):-
    adjacent([X, Y], Z), room(pit, A), Z==A,
    format('The following cell is breezy ~w', [[X, Y]]),nl.

% A room is glittery if the cell contains gold.
glittery([X, Y]):- 
   room(gold, [A, B]), X==A,Y==B.    



%  shooting the Wumpus from an adjacent room 
shootWumpus(AgentCell):-  
  wumpus([Xw, Yw]), 	
   AgentCell=[Xa, Ya], 
  (Yw==Ya ; Xw==Xa),			
    assert(noWumpus([Xw, Yw])),	
  format('~nThe Wumpus is found in room: ~w and the agent shot an arrow from cell ~w.~nThe WUMPUS has been killed!~n', [[Xw, Yw], AgentCell]),nl,nl,
  retractall(wumpus(_)),
    abort.


% Check if a room contains wumpus
explore(Cell, LeadingPath):-
   printStatus(Cell, LeadingPath),	
  glittery(Cell),
  append(LeadingPath, [Cell], CurrentPath),
 \+ wumpusPath(CurrentPath), assert(wumpusPath(CurrentPath)).

explore(Cell, _):- % Check if the agent can perceive breeze in a roonm
    breezy(Cell).
   

explore(Cell, _):-
  \+ breezy(Cell),
  adjacent(Cell, X),
  \+ noPit(X), assert(noPit(X)).

explore(Cell, _):-
  stenchy(Cell),
  adjacent(Cell, X),
  \+ noWumpus(X), assert(wumpus(X)).

explore(Cell, _):- % If the room doesn't have smell, record that the adjacent rooms certainly don't have the Wumpus.
  \+ stenchy(Cell),
  adjacent(Cell, X),
  \+ noWumpus(X), assert(noWumpus(X)),
  wumpus(Y), X==Y, retract(wumpus(Y)).

% try shooting the Wumpus from this cell,and find adjacent rooms that are safe and visit recursively.
explore(CurrentCell, LeadingPath):-
  (shootWumpus(CurrentCell); format('')),	% shoot Wumpus if possible.

  append(LeadingPath, [CurrentCell], CurrentPath),
  adjacent(CurrentCell, X), \+ member(X, LeadingPath),
  (( noWumpus(X), noPit(X)) -> write('');
    (\+ maybeVisitLater(CurrentCell, _) -> assert(maybeVisitLater(CurrentCell, LeadingPath)); write(''))
  ),
  noWumpus(X), noPit(X),
  explore(X, CurrentPath).



printStatus(Cell, LeadingPath):- % Print the status of any given room from during the search.
   \+ Cell = [1,1],
  forall(wumpus(X), format(' ~w',[X])),nl,
  write("KB: There is No pit in the following cells: "),
  %forall(noPit(Y), write(Y)),nl,
   forall(noPit(Y), format(' ~w',[Y])),nl,
  write("No Wumpus: "),
  forall(noWumpus(Z), format(' ~w',[Z])),nl,
  format('~n------------------------------~nCurrently in room ~w~nLeading path: ~w~n', [Cell, LeadingPath]),
  write("KB: There is No pit in the following cells: "),
  forall(noPit(Y), format(' ~w',[Y])),nl,
  write("No Wumpus: "),
  forall(noWumpus(Z), format(' ~w',[Z])),nl.
  %format('~n-----------------------~n~n').
printStatus(Cell, LeadingPath):-
    Cell = [1,1],
    format('~n------------------------------~nCurrently in room ~w~nLeading path: ~w~n', [Cell, LeadingPath]),
    forall(wumpus(X), format(' ~w',[X])),nl,
  write("KB: There is No pit in the following cells: "),
  forall(noPit(Y), format(' ~w',[Y])),nl,
  write("No Wumpus: "),
  forall(noWumpus(Z), format(' ~w',[Z])),nl.

