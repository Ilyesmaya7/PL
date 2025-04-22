% Define colors
color(red).
color(blue).
color(green).

% Define neighbors
nextTo(1,2).
nextTo(1,5).
nextTo(2,3).
nextTo(2,5).
nextTo(3,4).
nextTo(3,5).
nextTo(4,5).

% Symmetric neighbors
neighbors(X,Y) :- nextTo(X,Y), !.
neighbors(X,Y) :- nextTo(Y,X).

% Check if a coloring is invalid
invalid(hasColor(City,Color), [hasColor(X,Y)|_]) :-
    neighbors(City,X),
    Color = Y, !.
invalid(hasColor(City,Color), [_|T]) :-
    invalid(hasColor(City,Color), T).
invalid(_, []) :- fail.

% Find a valid solution
solution([], []).
solution([City|Rest], [hasColor(City,Color)|SolRest]) :-
    color(Color),
    solution(Rest, SolRest),
    \+ invalid(hasColor(City, Color), SolRest).

% Run the solver
prolog_solution(Solve) :-
    solution([1,2,3,4,5], Solve).
