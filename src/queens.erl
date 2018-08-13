-module(queens).

-import(lists,[seq/2,filter/2,sum/1]).

-export([queens/1,queens2/3,main/1]).

all_positions(Size) ->
    Seq = seq(1, Size),
    [ {X,Y} || X <- Seq, Y <- Seq].

attack_positions({Xq, Yq}, {X, Y}) ->
    Dx = abs(Xq - X),
    Dy = abs(Yq - Y),
    Xq == X orelse Yq == Y orelse Dx == Dy orelse (Dx == 1 andalso Dy == 2) orelse (Dx == 2 andalso Dy == 1).

queens(_, []) ->
    0;
queens(1, _) ->
    1;
queens(N, AvailablePositions) ->
    sum([ queens(N - 1, filter(fun(P) -> not attack_positions(Pos, P) end, AvailablePositions)) || Pos <- filter(fun({Xp, _}) -> Xp == N end, AvailablePositions)]).

queens(S) ->
    queens(S, all_positions(S)).

%% parallel version

queens2(S, Pos, Pid) ->
    AllPos = all_positions(S),
    N = queens(S - 1, filter(fun(P) -> not attack_positions(Pos, P) end, AllPos)),
    Pid ! N.

queens2(S, Pid) ->
    Ps = [ {S, Yp} || Yp <- seq(1, S)],
    lists:foreach(fun (P) -> spawn(?MODULE, queens2, [S, P, self()]) end, Ps),
    wait(0, S, Pid).

wait(Sum, 0, Pid) ->
    Pid ! Sum;
wait(Sum, N, Pid) ->
    receive
	S ->
	    wait(Sum + S, N - 1, Pid)
    end.

main([Arg , _]) -> % sequential
    N = queens(list_to_integer(Arg)),
    io:format("~p~n",[N]),
    init:stop();
main([Arg | _]) ->
    queens2(list_to_integer(Arg), self()),
    receive
	S ->
	    io:format("~p~n",[S])
    end,
    init:stop().
