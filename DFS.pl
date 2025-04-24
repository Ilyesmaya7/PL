:- use_module(library(clpfd)).

% --- Color list (1-Red,2-blue,3-green,4-yellow) ---
color(1).
color(2).
color(3).
color(4).

% ---Nodes in graph ---
node(a). node(b). node(c). node(d). node(e).
node(f). node(g). node(h).

% --- Edges ---
% Each `edge(X, Y)` declares an undirected edge between nodes X and Y.
% For example, edge(a, b) means "a" is connected to "b" (and vice versa because we dont need to check it twice).
edge(a,b). edge(a,c).
edge(b,c). edge(b,d).
edge(c,d). edge(c,e).
edge(d,f).
edge(e,f). edge(e,g).
edge(f,h).

% --- Main Predicate to Color the Graph ---
% This is the entry point. It finds all nodes and starts coloring using DFS.
colorGraph(ColorList) :-
    findall(X, node(X), Nodes),             % stack all nodes in 1 list named Nodes 
    colorGraphDFS(Nodes, [], ColorList).    % Start DFS algorithm coloring with an empty assignment

% --- DFS-Based Graph Coloring ---
% This recursively colors each node, one by one.
% Nodes: remaining nodes to color
% Acc: accumulator that holds the current color assignment
% ColorList: final result with all node-color pairs
colorGraphDFS([], Acc, Acc).  % Base case: no nodes left, return accumulated colors
colorGraphDFS([Node|Rest], Acc, ColorList) :-
    color(Color),                              % Choose a color
    \+ adjacentWithSameColor(Node, Color, Acc),% Make sure no neighbor has this color
    colorGraphDFS(Rest, [hasColor(Node, Color)|Acc], ColorList). % Assign color and recurse

% --- Check if a Neighbor Has the Same Color ---
% Used to enforce the coloring constraint that no two adjacent nodes share a color.
adjacentWithSameColor(Node, Color, ColorList) :-
    edge(Node, Neighbor),                                  % Check if Node is connected to Neighbor
    member(hasColor(Neighbor, Color), ColorList).          % If neighbor already has same color => conflict
adjacentWithSameColor(Node, Color, ColorList) :-
    edge(Neighbor, Node),                                  % Also check the reverse (since edges are undirected)
    member(hasColor(Neighbor, Color), ColorList).

% --- Print Colored Graph ---
% Helper function to print each node with its assigned color.
printColorList([]).  % Base case: nothing to print
printColorList([hasColor(Node, Color)|Rest]) :-
    format('Node ~w has color ~w~n', [Node, Color]),  % Print current node-color pair
    printColorList(Rest).                             % Recurse on the rest
