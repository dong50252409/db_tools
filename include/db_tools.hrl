%%%-------------------------------------------------------------------
%%% @author gz1417
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 一些定义
%%% @end
%%% Created : 03. 8月 2021 11:59
%%%-------------------------------------------------------------------
-define(CHECK(Expr), db_tools_util:check(Expr)).
-define(CHECK(Expr, Reason), db_tools_util:check(Expr, Reason)).
-define(VERBOSE(Str, Args), db_tools_dict:is_verbose() andalso ?CONSOLE(Str, Args)).
-define(CONSOLE(Str), io:format(Str ++ "~n")).
-define(CONSOLE(Str, Args), io:format(Str ++ "~n", Args)).
-define(IF(Condition, True, False), (case Condition of true -> True; false -> False end)).

-define(DEFAULT_HOST, <<"127.0.0.1">>).
-define(DEFAULT_PORT, 3306).
