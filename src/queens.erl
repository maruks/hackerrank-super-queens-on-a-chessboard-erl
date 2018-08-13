-module(queens).

-import(lists,[seq/2,filter/2,sum/1]).

-export([queens/1,queens_async/4,main/1]).

all_positions(Size) ->
    Seq = seq(1, Size),
    [ {X,Y} || X <- Seq, Y <- Seq].

attack_positions({Xq, Yq}, {X, Y}) ->
    Dx = abs(Xq - X),
    Dy = abs(Yq - Y),
    Xq == X orelse Yq == Y orelse Dx == Dy orelse (Dx == 1 andalso Dy == 2) orelse (Dx == 2 andalso Dy == 1).

queens(_, [], _) ->
    0;
queens(1, _, _) ->
    1;
queens(N, AvailablePositions, 0) ->
    sum([ queens(N - 1, filter(fun(P) -> not attack_positions(Pos, P) end, AvailablePositions), 0) || Pos <- filter(fun({Xp, _}) -> Xp == N end, AvailablePositions)]);
queens(N, AvailablePositions, Spawns) ->
    Moves = filter(fun({Xp, _}) -> Xp == N end, AvailablePositions),
    lists:foreach(fun (M) -> spawn(?MODULE, queens_async, [N - 1, filter(fun(P) -> not attack_positions(M, P) end, AvailablePositions), Spawns - 1,  self()]) end, Moves),
    wait(0, length(Moves)).

queens_async(N, Positions, Spawns, Pid) ->
    S = queens(N, Positions, Spawns),
    Pid ! S.

wait(Sum, 0) ->
    Sum;
wait(Sum, N) ->
    receive
	S ->
	    wait(Sum + S, N - 1)
    end.

queens(S, Spawns) ->
    queens(S, all_positions(S), Spawns).

queens(S) ->
    queens(S, all_positions(S), 0).

main([Size , Spawns]) ->
    N = queens(list_to_integer(Size), list_to_integer(Spawns)),
    io:format("~p~n",[N]),
    init:stop().
