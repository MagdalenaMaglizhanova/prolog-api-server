parent(magi, ivan).
parent(ivan, maria).
parent(ivan, lili).
grandparent(X, Y) :- parent(X, Z), parent(Z, Y).
