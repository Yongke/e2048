-module(matrix).

-export([init/0, to_list/1, from_list/1, feed/1, transpose/1,
         to_string/1, count_zero/1,
         move_left/1, move_right/1, move_up/1, move_down/1]).

-define(SIZE, 4).
-define(ELEM_CNT, ?SIZE * ?SIZE).
-define(INIT_NUMBER_CNT, 3). %% initial number is 2

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    Idxs = init_random(?ELEM_CNT, ?INIT_NUMBER_CNT),
    L = lists:map(
          fun(I) ->
                  case lists:member(I, Idxs) of
                      true -> 2;
                      _ -> 0
                  end
          end, lists:seq(1, ?ELEM_CNT)),
    from_list(L).

to_string(M) ->
    StrRows = lists:map(
                fun(Row) ->
                        lists:flatten(
                          ["|",
                           [string:join(
                              [integer_to_list(Num) || Num <- Row],
                              "||")],
                           "|"])
                end,
                M),
    string:join(StrRows, "\n").

to_list(M) ->
    lists:flatten(M).

from_list(L) ->
    [lists:sublist(L, 1, ?SIZE),
     lists:sublist(L, 5, ?SIZE),
     lists:sublist(L, 9, ?SIZE),
     lists:sublist(L, 13, ?SIZE)].

feed(M) ->
    L = lists:zip(lists:seq(1, ?ELEM_CNT), to_list(M)),
    L1 = lists:filter(
           fun({_, V}) when V =:= 0 -> true;
              (_) -> false
           end, L),
    {I, Val} = insert_random(length(L1)),
    {Idx, _} = lists:nth(I, L1),
    New = lists:map(
            fun({K, V}) when K =:= Idx->
                    {K, Val};
               (Keep) ->
                    Keep
            end, L),
    {_, New1} = lists:unzip(New),
    from_list(New1).

transpose([[]|_]) -> [];
transpose(M) ->
    [lists:map(fun hd/1, M) | transpose(lists:map(fun tl/1, M))].

move_left(M) ->
    M1 = lists:map(
           fun(Row) ->
                   merge_row_left(Row)
           end,
           M),
    assert_move(M1).

move_right(M) ->
    M1 = lists:map(
           fun(Row) ->
                   lists:reverse(merge_row_left(lists:reverse(Row)))
           end,
           M),
    assert_move(M1).


move_up(M) ->
    Mt = transpose(M),
    transpose(move_left(Mt)).

move_down(M) ->
    Mt = transpose(M),
    transpose(move_right(Mt)).

count_zero(M) ->
    length(lists:filter(
             fun(0) -> true;
                (_) -> false
             end,
             matrix:to_list(M))).

%%%===================================================================
%%% Internal functions
%%%===================================================================
init_random(Len, Cnt) ->
    Ts = {_, _, MicroSec} = now(),
    random:seed(Ts),
    random_aux(Len, Cnt, []).

insert_random(Len) ->
    Ts = {_, _, MicroSec} = now(),
    random:seed(Ts),
    Val = case MicroSec rem 2 of
              0 -> 2;
              _ -> 4
          end,
    {random:uniform(Len), Val}.

random_aux(Len, Cnt, Acc) when Cnt =:= 0 ->
    lists:reverse(Acc);
random_aux(Len, Cnt, Acc) ->
    R = random:uniform(Len),
    case lists:member(R, Acc) of
        true ->
            random_aux(Len, Cnt, Acc);
        _ ->
            random_aux(Len, Cnt - 1, [R | Acc])
    end.


merge_row_left(Row) ->
    merge_row_left(
      lists:filter(
        fun(0) -> false;
           (_) -> true
        end, Row), []).
    
merge_row_left([], Acc) ->
    lists:reverse(merge_padding(Acc));
merge_row_left([H1, H2 | Tail], Acc) when H1 =:= H2 ->
    lists:reverse(
      merge_padding(
        lists:reverse(Tail) ++ [H1 + H2 | Acc]));
merge_row_left([H | Tail], Acc) ->
    merge_row_left(Tail, [H | Acc]).

merge_padding(L) ->
    lists:duplicate(?SIZE - length(L), 0) ++ L.

assert_move(M) ->
    case count_zero(M) of
        0 ->
            throw(can_not_move);
        _ -> M
    end.
