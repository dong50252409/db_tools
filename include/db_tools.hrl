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
-define(CONSOLE(Str, Args), io:format(<<(unicode:characters_to_binary(io_lib:format(Str, Args)))/binary, "\n\n">>)).
-define(VERBOSE(Str, Args), db_tools_dict:is_verbose() andalso ?CONSOLE(Str, Args)).
-define(IF(Condition, True, False), (case Condition of true -> True; false -> False end)).

-define(DEFAULT_HOST, "localhost").
-define(DEFAULT_PORT, 3306).

-define(GET_VALUE(Key, List), proplists:get_value(Key, List)).
-define(GET_VALUE(Key, List, Default), proplists:get_value(Key, List, Default)).
-define(IS_DEFINED(Key, List), proplists:is_defined(Key, List)).

%% 工具运行模式
-define(MODE_UPDATE_DB, 1).     % 更新数据库以及表结构
-define(MODE_TRUNCATE_DB, 2).   % 截断数据库表（清库）
-define(MODE_GEN_MODEL, 3).    % 生成Erlang数据库表实体文件


-record(field_info, {
    name,
    default,
    type,
    comment,
    to_term
}).

-record(primary_key_info, {
    field_list = [],
    index_list = []
}).
