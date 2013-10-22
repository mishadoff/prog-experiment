-module(pingpong).

-export[ping/2, pong/0, start/0].

ping(0, PID) ->
    PID ! finished,
    io:format("Ping finished~n", []);

ping(N, PID) ->
    PID ! {ping, self()},
    receive
        pong ->
            io:format("Ping~n", [])
    end,
    ping(N - 1, PID).

pong() ->
    receive
        finished ->
            io:format("Pong finished~n", []);
        {ping, PID} ->
            io:format("Pong~n", []),
            PID ! pong,
            pong()
    end.

start() ->
    PID = spawn(pingpong, pong, []),
    spawn(pingpong, ping, [3, PID]).