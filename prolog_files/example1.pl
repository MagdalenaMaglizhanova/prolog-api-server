% Файл: example1.pl

% Факти за родители
parent(magi, ivan).
parent(ivan, petar).
parent(mari, ivan).
parent(petar, nina).
parent(nina, aleks).
parent(mari, vasil).
parent(vasil, kalina).

% Правило за баба/дядо
grandparent(X, Z) :-
    parent(X, Y),
    parent(Y, Z).

% Правило за баба
grandmother(X, Z) :-
    grandparent(X, Z),
    female(X).

% Правило за дядо
grandfather(X, Z) :-
    grandparent(X, Z),
    male(X).

% Факти за пол
female(magi).
female(mari).
female(nina).
female(kalina).

male(ivan).
male(petar).
male(vasil).
male(aleks).

% Правило за брат/сестра
sibling(X, Y) :-
    parent(Z, X),
    parent(Z, Y),
    X \= Y.

% Правило за прародител
ancestor(X, Z) :-
    parent(X, Z).

ancestor(X, Z) :-
    parent(X, Y),
    ancestor(Y, Z).

% Правило за потомък
descendant(Z, X) :-
    ancestor(X, Z).
