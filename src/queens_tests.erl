-module(queens_tests).
-import(queens,[queens/1]).
-include_lib("eunit/include/eunit.hrl").

queens_test() ->
    ?assertEqual(4, queens(10)),
    ?assertEqual(1876, queens(13)).
