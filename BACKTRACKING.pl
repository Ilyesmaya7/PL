:- use_module(library(clpfd)).

% --- Colors ---
color(1).  % Red
color(2).  % Blue
color(3).  % Green
color(4).  % Yellow

% --- Nodes ---
node(a). node(b). node(c). node(d). node(e).
node(f). node(g). node(h).

% --- Edges (undirected) ---
edge(a,b). edge(a,c).
edge(a,d). edge(a,e).
edge(b,c). edge(c,d).
edge(b,d).

edge(c,d). edge(c,e).
edge(d,f).
edge(e,f). edge(e,g).
edge(f,h).

% --- Entry Point ---
colorGraph(ColorList) :-
    findall(Node, node(Node), Nodes),
    assignColors(Nodes, [], ColorList).

% --- Backtracking Assignment ---
assignColors([], ColorList, ColorList).
assignColors([Node|Rest], AssignedSoFar, ColorList) :-
    co  lor(Color),
    \+ hasConflict(Node, Color, AssignedSoFar),  % Check this color is valid
    assignColors(Rest, [hasColor(Node, Color)|AssignedSoFar], ColorList).

% --- Conflict Checker ---
hasConflict(Node, Color, ColorList) :-
    edge(Node, Neighbor),
    member(hasColor(Neighbor, Color), ColorList).
hasConflict(Node, Color, ColorList) :-
    edge(Neighbor, Node),
    member(hasColor(Neighbor, Color), ColorList).

% --- Result Printer ---
printColorList([]).
printColorList([hasColor(Node, Color)|Rest]) :-
    format('Node ~w has color ~w~n', [Node, Color]),
    printColorList(Rest).

