%%%
%%% lpc_test.erl
%%%

-module(lpc_tests).

-include_lib("eunit/include/eunit.hrl").

%% Test Exports
-export([pmap_test_apply/2]).

%% Test MFA
pmap_test_apply(X, F) -> F(X).

%% Tests lpc:pmap/3
pmap_test() ->
    Res =
        lpc:pmap({lpc_tests, pmap_test_apply},
                [fun(X) -> timer:sleep(100), X end],
                lists:seq(1, 10000)),
    is_match(Res, lists:seq(1, 10000)).

%% is_match/2
%%
%% Verifies 2 lists are the exact same
is_match([], []) ->
    true;
is_match(List1, List2)
  when is_list(List1), is_list(List2) ->
    [E1 | Tail1] = List1,
    [E2 | Tail2] = List2,
    check(E1, E2),
    is_match(Tail1, Tail2).

%% check/2
%%
%% Asserts to make sure the two integer values are equal
check(Val1, Val2) when is_integer(Val1), is_integer(Val2) ->
    ?_assertEqual(Val1, Val2).
