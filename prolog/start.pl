% Test program

% Facts
male('John').
male('Bob').
male('Chris').
female('Alice').
female('Jessica').

father('John', 'Bob').
father('John', 'Chris').
father('John', 'Alice').
father('Abraham', 'John').

wife('Jessica', 'John').

% Rules
brothers(B1, B2) :-
        male(B1), male(B2),
        father(X, B1), father(X, B2).

sisters(S1, S2) :-
        female(S1), female(S2),
        father(X, S1), father(X, S1).

mother(M, S) :-
        female(M), wife(M, F), father(F, S).

son(S, M) :- male(S), mother(M, S).
son(S, F) :- male(S), father(F, S).

daughter(D, F) :- female(D), father(F, D).
daughter(D, M) :- female(D), mother(M, D).

husband(H, W) :- wife(W, H).

grandfather(G, S) :- father(G, X), father(X, S).