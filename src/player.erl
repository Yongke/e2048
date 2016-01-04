-module(player).

-export([start/0]).

-define(POSSIBLE_MOVES, [move_left, move_right, move_up, move_down]).

start() ->
    InitM = matrix:init(),
    format_output("I", InitM),
    play(InitM).


play(M) ->
    play(M, 10000).

play(_, 0) ->
    stop;
play(M, Cnt) ->
    ML = [{format_move(Move), matrix:Move(M)} || Move <- ?POSSIBLE_MOVES],
    SL = [{MoveTag, NewM, matrix:score(NewM)} || {MoveTag, NewM} <- ML],

    {MoveTag1, BestM, BestS} = hd(lists:reverse(lists:keysort(3, SL))),
    case BestS of
        0 -> io:format("E~n");
        _ ->
            BestM1 = matrix:feed(BestM),
            format_output(MoveTag1, BestM1),
            play(BestM1, Cnt - 1)
    end.


format_output(MoveTag, M) ->
    io:format("~s~n~s~n", [MoveTag, matrix:to_string(M)]).

format_move(move_left) ->
    "L";
format_move(move_right) ->
    "R";
format_move(move_up) ->
    "U";
format_move(move_down) ->
    "D".
