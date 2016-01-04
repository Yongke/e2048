-module(matrix).

-export([init/0, to_list/1, from_list/1, feed/1, transpose/1,
    to_string/1, count_zero/1, score/1,
    move_left/1, move_right/1, move_up/1, move_down/1]).

-define(SIZE, 4).
-define(ELEM_CNT, ?SIZE * ?SIZE).
-define(INIT_NUMBER_CNT, 3). %% initial number is 2

%% sort & transpose the initial weight matrix we got 8...
-define(WEIGHTS, [
    [[0.135759, 0.121925, 0.102812, 0.099937],
        [0.0997992, 0.0888405, 0.076711, 0.0724143],
        [0.060654, 0.0562579, 0.037116, 0.0161889],
        [0.0125498, 0.00992495, 0.00575871, 0.00335193]],
    [[0.135759, 0.0997992, 0.060654, 0.0125498],
        [0.121925, 0.0888405, 0.0562579, 0.00992495],
        [0.102812, 0.076711, 0.037116, 0.00575871],
        [0.099937, 0.0724143, 0.0161889, 0.00335193]],
    [[0.00335193, 0.00575871, 0.00992495, 0.0125498],
        [0.0161889, 0.037116, 0.0562579, 0.060654],
        [0.0724143, 0.076711, 0.0888405, 0.0997992],
        [0.099937, 0.102812, 0.121925, 0.135759]],
    [[0.00335193, 0.0161889, 0.0724143, 0.099937],
        [0.00575871, 0.037116, 0.076711, 0.102812],
        [0.00992495, 0.0562579, 0.0888405, 0.121925],
        [0.0125498, 0.060654, 0.0997992, 0.135759]],
    [[0.099937, 0.102812, 0.121925, 0.135759],
        [0.0724143, 0.076711, 0.0888405, 0.0997992],
        [0.0161889, 0.037116, 0.0562579, 0.060654],
        [0.00335193, 0.00575871, 0.00992495, 0.0125498]],
    [[0.0125498, 0.060654, 0.0997992, 0.135759],
        [0.00992495, 0.0562579, 0.0888405, 0.121925],
        [0.00575871, 0.037116, 0.076711, 0.102812],
        [0.00335193, 0.0161889, 0.0724143, 0.099937]],
    [[0.0125498, 0.00992495, 0.00575871, 0.00335193],
        [0.060654, 0.0562579, 0.037116, 0.0161889],
        [0.0997992, 0.0888405, 0.076711, 0.0724143],
        [0.135759, 0.121925, 0.102812, 0.099937]],
    [[0.099937, 0.0724143, 0.0161889, 0.00335193],
        [0.102812, 0.076711, 0.037116, 0.00575871],
        [0.121925, 0.0888405, 0.0562579, 0.00992495],
        [0.135759, 0.0997992, 0.060654, 0.0125498]]
]).


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
        fun({K, _}) when K =:= Idx ->
            {K, Val};
            (Keep) ->
                Keep
        end, L),
    {_, New1} = lists:unzip(New),
    from_list(New1).

possible_feeds(M) ->
    L = to_list(M),
    L1 = lists:zip(lists:seq(1, ?ELEM_CNT), L),
    L2 = lists:filter(
        fun({_, 0}) -> true;
            (_) -> false
        end,
        L1),
    LL =
        [lists:map(
            fun({Idx, _}) ->
                from_list(
                    lists:map(
                        fun({K, _}) when K =:= Idx ->
                            4;
                            ({_, Keep}) ->
                                Keep
                        end, L1))
            end,
            L2) || Val <- [2, 4]],
    lists:append(LL).

transpose(can_not_move) ->
    can_not_move;
transpose([[] | _]) -> [];
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

score(can_not_move) ->
    0;
score(M) ->
    A = score_aux(M),
    B = next_steps_score(M),
    A+B.


next_steps_score(M) ->
    Step = 2*(?ELEM_CNT - count_zero(M)) div ?SIZE,
    io:format("Steps: ~p", [Step]),
    next_steps_score(M, Step).

next_steps_score(_, 0) ->
    0;
next_steps_score(M0, Step) ->
    AllM = possible_feeds(M0),
    SAll = lists:foldl(
        fun(M, AllAcc) ->
            NextAllM = [move_left(M), move_right(M), move_up(M), move_down(M)],
            NextAllM1 = lists:filter(
                fun(can_not_move) -> false;
                    (_) -> true
                end, NextAllM),
            case NextAllM1 of
                [] ->
                    AllAcc;
                _ ->
                    S = pmap(
                        fun(NewM) ->
                            score_aux(NewM) + next_steps_score(NewM, Step - 1)
                        end,
                        NextAllM1),
                    S1 = hd(lists:reverse(lists:sort(S))),
                    AllAcc + S1
            end
        end,
        0,
        AllM),
    SAll / (2 * length(AllM)).

score_aux(M) ->
    lists:max(
        lists:map(
            fun(Weight) ->
                Ml = to_list(M),
                L = lists:zip(Ml, to_list(Weight)),
                Ws = lists:foldl(
                    fun({E, W}, Acc) ->
                        Acc + E * W
                    end,
                    0, L),
                Ws
            end,
            ?WEIGHTS)).

%%%===================================================================
%%% Internal functions
%%%===================================================================
init_random(Len, Cnt) ->
    Ts = now(),
    random:seed(Ts),
    random_aux(Len, Cnt, []).

insert_random(Len) ->
    Ts = now(),
    random:seed(Ts),
    Val = random:uniform(2) * 2,
    {random:uniform(Len), Val}.

random_aux(_, Cnt, Acc) when Cnt =:= 0 ->
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
        0 -> can_not_move;
        _ -> M
    end.

pmap(F, L) ->
    S = self(),
    Pids = lists:map(
        fun(I) ->
            spawn(fun() -> do_f(S, F, I) end)
        end, L),
    gather(Pids).

gather([H|T]) ->
    receive
        {H, Ret} ->
            [Ret|gather(T)]
    end;
gather([]) ->
    [].

do_f(Parent, F, I) ->
    Parent ! {self(), catch F(I)}.
