-module(matrix_test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

basic_test() ->
    M = matrix:init(),
    ?assertEqual(4, length(M)),
    [ Row1 | _] = M,
    ?assertEqual(4, length(Row1)),

    L = matrix:to_list(M),
    M1 = matrix:from_list(L),
    ?assertEqual(M, M1),

    Mt = matrix:transpose(M),
    ?assertEqual(M, matrix:transpose(Mt)),

    Mf = matrix:feed(M),

    ?assertEqual(matrix:count_zero(M) - 1, matrix:count_zero(Mf)), 

    ?assertEqual("|0||0||0||0|\n|2||0||0||0|\n|0||2||2||0|\n|0||0||0||0|",
                 matrix:to_string([[0,0,0,0],[2,0,0,0],[0,2,2,0],[0,0,0,0]])),
    ok.


move_test() ->
    M = [[0,0,0,0],[2,0,0,0],[0,2,2,0],[0,0,0,0]],
    M1 = [[0,2,2,0],[2,2,0,0],[2,0,0,2],[4,4,2,4]],
    M2 = [[8,8,8,8],[2,2,2,2],[4,4,4,4],[16,16,16,16]],

    ?assertEqual([[4,0,0,0],[4,0,0,0],[4,0,0,0],[8,2,4,0]],
                 matrix:move_left(M1)),
    ?assertEqual([[0,0,0,0],[2,0,0,0],[4,0,0,0],[0,0,0,0]],
                 matrix:move_left(M)),
    ?assertEqual([[16,8,8,0],[4,2,2,0],[8,4,4,0],[32,16,16,0]],
                 matrix:move_left(M2)),
    
    ?assertEqual([[0,0,0,0],[0,0,0,2],[0,0,0,4],[0,0,0,0]],
                 matrix:move_right(M)),
    ?assertEqual([[0,0,0,4],[0,0,0,4],[0,0,0,4],[0,8,2,4]],
                 matrix:move_right(M1)),
    ?assertEqual([[0,8,8,16],[0,2,2,4],[0,4,4,8],[0,16,16,32]],
                 matrix:move_right(M2)),

    ?assertEqual([[2,2,2,0],[0,0,0,0],[0,0,0,0],[0,0,0,0]],
                 matrix:move_up(M)),
    ?assertEqual([[4,4,4,2],[4,4,0,4],[0,0,0,0],[0,0,0,0]],
                 matrix:move_up(M1)),
    ?assertThrow(can_not_move, matrix:move_up(M2)),


    ?assertEqual([[0,0,0,0],[0,0,0,0],[0,0,0,0],[2,2,2,0]],
                 matrix:move_down(M)),
    ?assertEqual([[0,0,0,0],[0,0,0,0],[4,4,0,2],[4,4,4,4]],
                 matrix:move_down(M1)),
    ?assertThrow(can_not_move, matrix:move_down(M2)),
    
    ok.


