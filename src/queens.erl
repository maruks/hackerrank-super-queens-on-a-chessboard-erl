-module(queens).

-import(lists,[seq/2,filter/2, sum/1]).

-export([queens/1,queens2/3,main/1,main2/1]).

all_positions(Size) ->
    Seq = seq(1,Size),
    [ {X,Y} || X <- Seq, Y <- Seq].

attack_positions(Size, {Xp,Yp}) ->
    Knight = [ {Xp + 1, Yp + Y} || Y <- [-2, 2]] ++ [ {Xp + 2,Yp + Y} || Y <- [-1, 1]],
    Rook = [ {Xp, Y} || Y <- seq(1, Size)] ++ [ {X, Yp} || X <- seq(Xp, Size)],
    Bishop = [ {Xp + D, Yp + D} || D <- seq(1, min(Size - Yp, Size - Xp)) ] ++ [ {Xp + D, Yp - D} || D <-  seq(1, min(Yp, Size - Xp))],
    Rook ++ filter(fun ({X, Y}) -> X > 0 andalso Y > 0 andalso X =< Size andalso Y =< Size end, Knight ++ Bishop).

queens(_, _, []) ->
    0;
queens(Size, Size, Positions) ->
    length(Positions);
queens(N, Size, AvailablePositions) ->
    sum([ queens(N + 1, Size,  AvailablePositions -- attack_positions(Size, P)) || P <- filter(fun({Xp, _}) -> Xp == N end, AvailablePositions) ] ).

queens(S) ->
    queens(1, S, all_positions(S)).

main([Arg | _]) ->
    N = queens(list_to_integer(Arg)),
    io:format("~p~n",[N]),
    init:stop().

%% parallel version

queens2(S, Ps, Pid) ->
    N = queens(1, S, Ps),
    Pid ! N.

queens2(S, Pid) ->
    AllPos = all_positions(S),
    Ps = [ filter( fun({X, Y}) -> X > 1 orelse Y == Yp end, AllPos) || Yp <- seq(1, S)],
    lists:foreach(fun (P) -> spawn(?MODULE, queens2, [S, P, self()]) end, Ps),
    wait(0, S, Pid).

wait(Sum, 0, Pid) ->
    Pid ! Sum;
wait(Sum, N, Pid) ->
    receive
	S ->
	    %% io:format("~p~n",[S]),
	    wait(Sum + S, N - 1, Pid)
    end.

main2([Arg | _]) ->
    queens2(list_to_integer(Arg), self()),
    receive
	S ->
	    io:format("~p~n",[S])
    end,
    init:stop().
