%%%-------------------------------------------------------------------
%%% @author gz1417
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 操作数据库
%%% @end
%%% Created : 06. 8月 2021 16:23
%%%-------------------------------------------------------------------
-module(db_tools_operation).

-include("db_tools.hrl").

%% API
-export([do_connect_db/0, do_create_db/0, do_use_database/0, do_create_tables/1, do_alter_tables/1]).

query(SQL) ->
    query(SQL, []).
query(SQL, Params) ->
    ?VERBOSE("SQL:~ts~n Parmas:~w", [SQL, Params]),
    Conn = db_tools_dict:get_db_conn(),
    BinSQL = unicode:characters_to_binary(SQL),
    Result = ?CHECK(mysql:query(Conn, BinSQL, Params)),
    db_tools:do_export_db(BinSQL),
    Result.

do_connect_db() ->
    Args = [
        {host, db_tools_dict:get_db_host()},
        {port, db_tools_dict:get_db_port()},
        {user, db_tools_dict:get_db_user()},
        {password, db_tools_dict:get_db_passwd()}
    ],
    case mysql:start_link(Args) of
        {ok, Conn} ->
            db_tools_dict:set_db_conn(Conn);
        {error, Reason} ->
            throw(Reason)
    end,
    ok.

do_create_db() ->
    DBName = db_tools_dict:get_db_name(),
    SQL = io_lib:format("CREATE DATABASE IF NOT EXISTS `~s`;", [DBName]),
    query(SQL).

do_use_database() ->
    Conn = db_tools_dict:get_db_conn(),
    DBUser = db_tools_dict:get_db_user(),
    DBPasswd = db_tools_dict:get_db_passwd(),
    Args = [{database, atom_to_list(db_tools_dict:get_db_name())}],
    mysql:change_user(Conn, DBUser, DBPasswd, Args),
    ok.

do_create_tables({_, Tables}) ->
    Fun = fun(TableInfo) -> do_create_table(TableInfo) end,
    lists:foreach(Fun, Tables).

do_create_table(TableInfo) ->
    TableName = get_table_name(TableInfo),
    FieldsStr = get_fields(TableName, TableInfo),
    IndexStr = get_index_list(TableInfo),
    Options = get_options(TableInfo),
    {SQL, Args} = {"CREATE TABLE IF NOT EXISTS `~ts` (~n~ts", [TableName, FieldsStr]},
    {SQL1, Args1} = ?IF(IndexStr =:= [], {SQL ++ "~n) ", Args}, {SQL ++ ",~n~ts\n) ", Args ++ [IndexStr]}),
    {SQL2, Args2} = ?IF(Options =:= [], {SQL1 ++ ";", Args1}, {SQL1 ++ "~ts;", Args1 ++ [Options]}),
    SQL4 = io_lib:format(SQL2, Args2),
    query(SQL4).

get_table_name(TableInfo) ->
    TableOptions = ?CHECK(proplists:get_value(table, TableInfo, {error, <<"配置表未指定表选项"/utf8>>})),
    ?CHECK(proplists:get_value(name, TableOptions, {error, <<"配置表未指定表名"/utf8>>})).

get_fields(TableName, TableInfo) ->
    Fields = ?CHECK(proplists:get_value(fields, TableInfo, {error, <<"配置表未指定表字段列"/utf8>>})),
    TableNameStr = unicode:characters_to_binary(db_tools_util:any_to_list(TableName)),
    lists:flatten(lists:join(",\n", get_fields_1(TableNameStr, Fields))).

get_fields_1(TableName, [Field | T]) ->
    Result = concat_fields(TableName, Field, [name, type, not_null, default, auto_inc, comment]),
    [["\t", lists:join(" ", Result)] | get_fields_1(TableName, T)];
get_fields_1(_TableName, []) ->
    [].

concat_fields(TableName, Field, [name | T]) ->
    Name = ?CHECK(proplists:get_value(name, Field, {error, <<"表："/utf8, TableName/binary, " 未指定name选项，请检查"/utf8>>})),
    [["`", atom_to_list(Name), "`"] | concat_fields(TableName, Field, T)];
concat_fields(TableName, Field, [type | T]) ->
    Type = ?CHECK(proplists:get_value(type, Field, {error, <<"表："/utf8, TableName/binary, " 未指定type选项，请检查"/utf8>>})),
    [[Type] | concat_fields(TableName, Field, T)];
concat_fields(TableName, Field, [not_null | T]) ->
    ?IF(proplists:is_defined(not_null, Field), ["NOT NULL"], []) ++ concat_fields(TableName, Field, T);
concat_fields(TableName, Field, [default | T]) ->
    case proplists:get_value(default, Field) of
        undefined ->
            concat_fields(TableName, Field, T);
        Default ->
            [["DEFAULT '", db_tools_util:any_to_list(Default), "'"] | concat_fields(TableName, Field, T)]
    end;
concat_fields(TableName, Field, [auto_inc | T]) ->
    ?IF(proplists:is_defined(auto_inc, Field), ["AUTO_INCREMENT"], []) ++ concat_fields(TableName, Field, T);
concat_fields(TableName, Field, [comment | T]) ->
    case proplists:get_value(comment, Field) of
        undefined ->
            concat_fields(TableName, Field, T);
        Comment ->
            [["COMMENT '", Comment, "'"] | concat_fields(TableName, Field, T)]
    end;
concat_fields(_TableName, _Field, []) ->
    [].

get_index_list(TableInfo) ->
    IndexList = proplists:get_value(index, TableInfo, []),
    lists:flatten(lists:join(",\n", get_index_list_1(IndexList))).

get_index_list_1([Index | T]) ->
    Result = concat_index(Index, [type, name, fields, func, comment]),
    [["\t", lists:join(" ", Result)] | get_index_list_1(T)];
get_index_list_1([]) ->
    [].

concat_index(Index, [type | T]) ->
    [?IF(proplists:is_defined(primary, Index), ["PRIMARY KEY"],
        ?IF(proplists:is_defined(unique, Index), ["UNIQUE KEY"], ["KEY"]))
        | concat_index(Index, T)];
concat_index(Index, [name | T]) ->
    case proplists:get_value(name, Index) of
        undefined ->
            concat_index(Index, T);
        Name ->
            [["`", atom_to_list(Name), "`"] | concat_index(Index, T)]
    end;
concat_index(Index, [fields | T]) ->
    Fields = proplists:get_value(fields, Index),
    FieldsStr = [["`", atom_to_list(Field), "`"] || Field <- Fields],
    [["(", lists:join(",", FieldsStr), ")"] | concat_index(Index, T)];
concat_index(Index, [func | T]) ->
    ?IF(proplists:is_defined(btree, Index), ["USING BTREE"],
        ?IF(proplists:is_defined(hash, Index), ["USING HASH"], [])) ++ concat_index(Index, T);
concat_index(Index, [comment | T]) ->
    case proplists:get_value(comment, Index) of
        undefined ->
            concat_index(Index, T);
        Comment ->
            [["COMMENT '", Comment, "'"] | concat_index(Index, T)]
    end;
concat_index(_Index, []) ->
    [].

get_options(TableInfo) ->
    TableOptions = ?CHECK(proplists:get_value(table, TableInfo, {error, <<"配置表未指定表选项"/utf8>>})),
    Result = concat_options(TableOptions, [engine, charset, collate, comment]),
    lists:flatten(lists:join(" ", Result)).

concat_options(TableOptions, [engine | T]) ->
    case proplists:get_value(engine, TableOptions) of
        undefined ->
            concat_options(TableOptions, T);
        Engine ->
            [["ENGINE=", Engine, " DEFAULT"] | concat_options(TableOptions, T)]
    end;
concat_options(TableOptions, [charset | T]) ->
    case proplists:get_value(charset, TableOptions) of
        undefined ->
            concat_options(TableOptions, T);
        Charset ->
            [["CHARSET=", Charset] | concat_options(TableOptions, T)]
    end;
concat_options(TableOptions, [collate | T]) ->
    case proplists:get_value(collate, TableOptions) of
        undefined ->
            concat_options(TableOptions, T);
        Collate ->
            [["COLLATE=", Collate] | concat_options(TableOptions, T)]
    end;
concat_options(TableOptions, [comment | T]) ->
    case proplists:get_value(comment, TableOptions) of
        undefined ->
            concat_options(TableOptions, T);
        Comment ->
            [["COMMENT='", Comment, "'"] | concat_options(TableOptions, T)]
    end;
concat_options(_TableOptions, []) ->
    [].

do_alter_tables({_, Tables}) ->
    Fun = fun(TableInfo) -> do_alter_table(TableInfo) end,
    lists:foreach(Fun, Tables).

do_alter_table(TableInfo) ->
    TableName = get_table_name(TableInfo),
    get_modify_fields(TableName, TableInfo),
    get_delete_fields(TableInfo),
    get_add_fields(TableInfo),
    get_modify_index(TableInfo),
    get_delete_index(TableInfo),
    get_add_index(TableInfo),
    get_modify_options(TableInfo),
    SQL = "",
    query(SQL),
    ok.

get_modify_fields(TableName, TableInfo) ->
    Fields = ?CHECK(proplists:get_value(fields, TableInfo, {error, <<"配置表未指定表字段列"/utf8>>})),
    SQL = io_lib:format("DESC `~ts`;", [TableName]),
    {ok, _Fields, ValuesList} = query(SQL),
    lists:foldl(
        fun([Field, Type, Null, Key, Default, Extra], AccMap) ->
            AccMap#{
                field => Field, type => Type,
                null => Null, key => Key,
                default => Default, extra => Extra
            }
        end, #{}, ValuesList).

get_delete_fields(TableInfo) ->
    erlang:error(not_implemented).

get_modify_index(TableInfo) ->
    erlang:error(not_implemented).

get_add_index(TableInfo) ->
    erlang:error(not_implemented).

get_add_fields(TableInfo) ->
    erlang:error(not_implemented).

get_delete_index(TableInfo) ->
    erlang:error(not_implemented).

get_modify_options(TableInfo) ->
    erlang:error(not_implemented).