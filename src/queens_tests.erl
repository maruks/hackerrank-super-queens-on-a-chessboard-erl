-module(queens_tests).
-import(queens,[queens/1]).
-include_lib("eunit/include/eunit.hrl").

queens_test() ->
    ?assertEqual(4, queens(10)),
    ?assertEqual(44, queens(11)),
    ?assertEqual(156, queens(12)),
    ?assertEqual(1876, queens(13)),
    ?assertEqual(5180, queens(14)).
