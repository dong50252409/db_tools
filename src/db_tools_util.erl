%%%-------------------------------------------------------------------
%%% @author gz1417
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 8月 2021 11:11
%%%-------------------------------------------------------------------
-module(db_tools_util).

-include("db_tools.hrl").

%% API
-export([check/1, check/2, any_to_list/1, any_to_atom/1, key_take/2, key_find/2, key_del/2, run_fun_list/1]).

check(Expr) ->
    case Expr of
        {error, Reason} ->
            throw(Reason);
        Value ->
            Value
    end.

check(Expr, Reason) ->
    case Expr of
        undefined ->
            throw(Reason);
        Value ->
            Value
    end.

any_to_list(Any) when is_list(Any) ->
    Any;
any_to_list(Any) when is_binary(Any) ->
    binary_to_list(Any);
any_to_list(Any) when is_atom(Any) ->
    atom_to_list(Any);
any_to_list(Any) when is_integer(Any) ->
    integer_to_list(Any);
any_to_list(Any) when is_float(Any) ->
    float_to_list(Any).

any_to_atom(Any) when is_binary(Any) ->
    binary_to_atom(Any);
any_to_atom(Any) when is_list(Any) ->
    list_to_atom(Any).


key_take(Key, [H | T]) ->
    ?IF(lists:member(Key, H), {H, T}, key_take(Key, T));
key_take(_Key, []) ->
    false.

key_find(Key, [H | T]) ->
    ?IF(lists:member(Key, H), H, key_find(Key, T));
key_find(_Key, []) ->
    false.

key_del(Key, [H | T]) ->
    ?IF(lists:member(Key, H), T, [H | key_del(Key, T)]);
key_del(_Key, []) ->
    [].


run_fun_list([{Fun, Args} | T]) ->
    case erlang:apply(Fun, Args) of
        true ->
            run_fun_list(T);
        Other ->
            Other
    end;
run_fun_list([]) ->
    true.