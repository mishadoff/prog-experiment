-module(tutorial).
-export([double/1, fac/1, mult/2, convert/2, convert_tuple/1,
         list_length/1, reverse_list/1, your_age/1, say_something/2, start/0
]).


% Basic Numbers

double(X) ->
    2 * X.

fac(1) ->
    1;
fac(N) ->
    N * fac(N - 1).

% Variables (Capitalized word)

mult(X, Y) ->
   X * Y.

% Atoms (small first letter)

convert(X, inch) ->
    X / 2.54;
convert(X, cent) ->
    X * 2.54.

% Tuples {}

convert_tuple({cent, X}) ->
    {inch, X / 2.54};
convert_tuple({inch, Y}) ->
    {cent, Y * 2.54}.

% Lists []

list_length([]) ->
    0;
list_length([_|T]) ->
    1 + list_length(T).

% No strings!? WTF [97, 98, 99] = "abc"

% Reverse List

reverse_list(List) ->
    reverse_list(List, []).

reverse_list([H | T], A) ->
    reverse_list(T, [H | A]);
reverse_list([], A) ->
    A.

% Ifs

your_age(Age) ->
    if
        Age < 10 ->
	    io:format("Child~n", []);
	Age < 20 ->
            io:format("Young~n", []);
        Age < 50 ->
            io:format("Adult~n", []);
	Age < 100 ->
	    io:format("Old Fuck~n", []);
	true ->
  	    io:format("It is time to die!~n")
    end.

% Processes

say_something(_, 0) ->
    done;
say_something(What, Times) ->
    io:format("~p~n", [What]),
    say_something(What, Times - 1).

start() ->
    spawn(tutorial, say_something, [hello, 3]),
    spawn(tutorial, say_something, [goodbye, 3]).
