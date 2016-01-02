-module(matrix_test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

basic_test() ->
    M = matrix:init(),
    ?debugVal(M),
    ?assertEqual(4, length(M)),
    [ Row1 | _] = M,
    ?assertEqual(4, length(Row1)),

    L = matrix:to_list(M),
    M1 = matrix:from_list(L),
    ?assertEqual(M, M1),

    Mt = matrix:transpose(M),
    ?debugVal(Mt),
    ?assertEqual(M, matrix:transpose(Mt)),

    Mf = matrix:feed(M),
    ?debugVal(Mf),
    
    C = zero_cnt(M),
    Cf = zero_cnt(Mf),
    ?assertEqual(C - 1, Cf), 
    ok.

zero_cnt(M) ->
    length(lists:filter(
             fun(0) -> true;
                (_) -> false
             end,
             matrix:to_list(M))).
