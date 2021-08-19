%%%-------------------------------------------------------------------
%%% @author gz1417
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. 8月 2021 14:50
%%%-------------------------------------------------------------------
-module(db_tools_table).

-include("db_tools.hrl").

%% API
-export([
    get_table_options/1, get_table_name/1, get_table_comment/1, get_table_fields/1,
    get_field_name/1, get_field_type/1,
    get_table_index_list/1, get_index_fields/1,
    get_table_extend_fields/1]).

%% 获取表选项
get_table_options(TableInfo) ->
    ?CHECK(?GET_VALUE(table, TableInfo, {error, <<"未指定表选项，请检查"/utf8>>})).

%% 获取表名
get_table_name(TableInfo) ->
    TableOptions = get_table_options(TableInfo),
    AtomTableName = ?CHECK(?GET_VALUE(name, TableOptions, {error, <<"未指定表名，请检查"/utf8>>})),
    unicode:characters_to_binary(atom_to_binary(AtomTableName)).

%% 获取表描述
get_table_comment(TableInfo) ->
    TableOptions = get_table_options(TableInfo),
    ?GET_VALUE(comment, TableOptions).

%% 获取表字段列表
get_table_fields(TableInfo) ->
    ?CHECK(?GET_VALUE(fields, TableInfo, {error, <<"未指定表字段列，请检查"/utf8>>})).

%% 获取字段名
get_field_name(Field) ->
    AtomFieldName = ?CHECK(?GET_VALUE(name, Field, {error, <<"未指定字段name选项，请检查"/utf8>>})),
    unicode:characters_to_binary(atom_to_binary(AtomFieldName)).

%% 获取字段类型
get_field_type(Field) ->
    ?CHECK(?GET_VALUE(type, Field, {error, <<"未指定type选项，请检查"/utf8>>})).

%% 获取表索引列表
get_table_index_list(TableInfo) ->
    ?GET_VALUE(index, TableInfo, []).

%% 获取索引字段列
get_index_fields(IndexInfo) ->
    ?CHECK(?GET_VALUE(fields, IndexInfo, {error, <<"未指定表索引字段列，请检查"/utf8>>})).

%% 获取表额外字段列表
get_table_extend_fields(TableInfo) ->
    ?GET_VALUE(extend_fields, TableInfo, []).


