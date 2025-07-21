parent(magi, ivan).
parent(ivan, maria).
grandparent(X, Y) :- parent(X, Z), parent(Z, Y).
