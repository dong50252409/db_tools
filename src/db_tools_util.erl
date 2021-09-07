%%%-------------------------------------------------------------------
%%% @author gz1417
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 工具集合
%%% @end
%%% Created : 05. 8月 2021 11:11
%%%-------------------------------------------------------------------
-module(db_tools_util).

-include("db_tools.hrl").

%% API
-export([check/1, check/2, any_to_list/1, any_to_atom/1, run_fun_list/1]).

-spec check(Expr :: term()) -> no_return()|term().
check(Expr) ->
    case Expr of
        {error, Reason} ->
            throw(Reason);
        Value ->
            Value
    end.

-spec check(Expr :: term(), Reason :: term()) -> no_return()|term().
check(Expr, Reason) ->
    case Expr of
        undefined ->
            throw(Reason);
        Value ->
            Value
    end.

-spec any_to_list(Any :: list()|binary()|atom()|integer()|float()) -> list().
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

-spec any_to_atom(Any :: list()|binary()|atom()|integer()|float()) -> atom().
any_to_atom(Any) when is_list(Any) ->
    list_to_atom(Any);
any_to_atom(Any) when is_binary(Any) ->
    binary_to_atom(Any, utf8);
any_to_atom(Any) when is_atom(Any) ->
    Any;
any_to_atom(Any) ->
    list_to_atom(any_to_list(Any)).

-spec run_fun_list([{function(), [term()]}]) -> true|term().
run_fun_list([{Fun, Args} | T]) ->
    case erlang:apply(Fun, Args) of
        true ->
            run_fun_list(T);
        Other ->
            Other
    end;
run_fun_list([]) ->
    true.
