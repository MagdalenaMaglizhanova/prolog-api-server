% Файл: example1.pl

% Факти за родители
parent(magi, ivan).
parent(ivan, petar).
parent(mari, ivan).

% Правило за баба/дядо
grandparent(X, Z) :-
    parent(X, Y),
    parent(Y, Z).
