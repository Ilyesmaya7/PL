:- use_module(library(clpfd)).

% --- Color list (1-Red, 2-Blue, 3-Green, 4-Yellow) ---
color(1).
color(2).
color(3).
color(4).

% --- Graph Nodes ---
node(a). node(b). node(c). node(d). node(e).
node(f). node(g). node(h).

% --- Undirected Edges ---
edge(a,b). edge(a,c).
edge(b,c). edge(b,d).
edge(c,d). edge(c,e).
edge(d,f).
edge(e,f). edge(e,g).
edge(f,h).

% --- Entry point: greedy coloring ---
colorGraph(ColorList) :-
    findall(Node, node(Node), Nodes),           % Get list of all nodes
    greedyColorNodes(Nodes, [], ColorList).     % Start greedy coloring

% --- Greedy coloring implementation ---
% Base case: no more nodes to color
greedyColorNodes([], Acc, Acc).

% Recursive step
greedyColorNodes([Node|Rest], Acc, ColorList) :-
    findall(C, color(C), Colors),                                       % Available colors
    exclude(adjacentHasColor(Node, Acc), Colors, ValidColors),         % Filter out invalid colors
    ValidColors = [ChosenColor|_],                                     % Take the first valid color
    greedyColorNodes(Rest, [hasColor(Node, ChosenColor)|Acc], ColorList). % Assign it and continue

% --- Check if a color is invalid due to a neighbor using it ---
adjacentHasColor(Node, ColorList, Color) :-
    ( edge(Node, Neighbor) ; edge(Neighbor, Node) ),                   % Check undirected edge
    member(hasColor(Neighbor, Color), ColorList).                      % Neighbor has same color?

% --- Print Colored Graph ---
printColorList([]).
printColorList([hasColor(Node, Color)|Rest]) :-
    format('Node ~w has color ~w~n', [Node, Color]),
    printColorList(Rest).
