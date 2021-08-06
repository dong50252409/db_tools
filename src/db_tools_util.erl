%%%-------------------------------------------------------------------
%%% @author gz1417
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 8月 2021 11:11
%%%-------------------------------------------------------------------
-module(db_tools_util).
-author("gz1417").

%% API
-export([check/1, check/2, any_to_list/1]).

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

