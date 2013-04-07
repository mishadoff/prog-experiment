% Test program

% Facts
male('John').
male('Bob').
male('Chris').

father('John', 'Bob').
father('John', 'Chris').

% Rules
brothers(B1, B2) :-
        father(X, B1), father(X, B2).