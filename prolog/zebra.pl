%1.	There are five work places in a row.
%2.	Srikantachari's workplace is immediately to the right of the Linus's workplace.
%3.	Graddle was pushed by management to be used with mercurial.
%4.	Vasya is maven guru.
%5.	The Cobol is used with SBT.
%6.	CruiseControl is being run at the middle place.
%7.	The C++ code is produced in the first workplace.
%8.	Scala being used on same place with Perforce.
%9.	Old fashion Ant is used in the workplace next to the one, where SVN is installed.
%10.	Martin likes to use Python
%11.	The Ivy user keeps using Jenkins.
%12.	Srikantachari loves to use Hudson.
%13.	Java perfectly fits to TeamCity.
%14.	The C++ used at the workplace, next to the Josh's one.
%15.	Maven is used by the neighbor of the git fanboy.
%16.	There although Maven and IBM ClearCase are used. Somewhere!


work(name, lang, ci, build, cvs).

% Rules

is_right(L, R, [L | [R | _]]).
is_right(L, R, [_ | Rest]) :- is_right(L, R, Rest).

next_to(X, Y, List) :- is_right(X, Y, List).
next_to(X, Y, List) :- is_right(Y, X, List).

% 1. There are five work places in a row.
Workplaces = [W1, W2, W3, W4, W5].

% 3
member(work(_, _, _, gradle, mercurial), Workplaces).
% 4
member(work(vasya, _, _, maven, _), Workplaces).
% 5
member(work(_, cobol, _, sbt, _), Workplaces).
% 6
work(_, _, cruise_control, _, _) = W3.
