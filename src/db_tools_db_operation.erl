%%%-------------------------------------------------------------------
%%% @author gz1417
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 操作数据库
%%% @end
%%% Created : 06. 8月 2021 16:23
%%%-------------------------------------------------------------------
-module(db_tools_db_operation).

-include("db_tools.hrl").

%% API
-export([do_close_io/0, do_connect_db/0, do_create_db/0, do_create_tables/1, do_alter_tables/1]).

-spec do_close_io() -> ok.
do_close_io() ->
    case db_tools_dict:get_export_io() of
        undefined ->
            ok;
        IO when is_pid(IO) ->
            file:close(IO)
    end,
    ok.

%% 执行导出SQL语句到文件
do_export_sql(SQL) ->
    case db_tools_dict:get_export_io() of
        undefined ->
            ok;
        IO when is_pid(IO) ->
            io:format(IO, "~ts\n\n", [SQL])
    end.

%% 执行一条SQL查询
query(SQL) ->
    query(SQL, []).
query(SQL, Params) ->
    Conn = db_tools_dict:get_db_conn(),
    BinSQL = unicode:characters_to_binary(SQL),
    ?VERBOSE("SQL:~ts", [BinSQL]),
    ?CHECK(mysql:query(Conn, BinSQL, Params)).

%% 执行一条SQL语句
execute(SQL) ->
    execute(SQL, []).
execute(SQL, Params) ->
    Conn = db_tools_dict:get_db_conn(),
    BinSQL = unicode:characters_to_binary(SQL),
    ?VERBOSE("SQL:~ts", [BinSQL]),
    Result = ?CHECK(mysql:query(Conn, BinSQL, Params)),
    do_export_sql(BinSQL),
    Result.

%% 执行连接数据库
-spec do_connect_db() -> ok|no_return().
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

%% 执行创建数据库
-spec do_create_db() -> ok|no_return().
do_create_db() ->
    DBNameStr = db_tools_dict:get_db_name(),
    SQL = io_lib:format("SELECT `SCHEMA_NAME` FROM `information_schema`.`SCHEMATA` WHERE `SCHEMA_NAME`='~ts';", [DBNameStr]),
    case query(SQL) of
        {ok, _Fields, [[DBNameStr | _]]} ->
            ok;
        {ok, _Fields, _Values} ->
            CreateDBSQL = io_lib:format("CREATE DATABASE `~ts` ~ts;", [DBNameStr, get_db_options()]),
            execute(CreateDBSQL),
            ok
    end.

%% 拼接数据库表选项
get_db_options() ->
    OptionsList = concat_db_option([character, collation]),
    lists:flatten(lists:join(" ", OptionsList)).

concat_db_option([character | T]) ->
    case db_tools_dict:get_db_character() of
        undefined ->
            concat_db_option(T);
        Character ->
            [["DEFAULT CHARACTER SET ", Character] | concat_db_option(T)]
    end;
concat_db_option([collation | T]) ->
    case db_tools_dict:get_db_collation() of
        undefined ->
            concat_db_option(T);
        Collation ->
            [["DEFAULT COLLATE ", Collation] | concat_db_option(T)]
    end;
concat_db_option([]) ->
    [].

%% 执行创建数据库表结构
-spec do_create_tables(Config :: list()) -> ok|no_return().
do_create_tables(Config) ->
    Fun = fun(TableInfo) -> do_create_table(TableInfo) end,
    lists:foreach(Fun, Config).

do_create_table(TableInfo) ->
    TableNameStr = db_tools_table:get_table_name(TableInfo),
    case is_not_table_exist(TableNameStr) of
        true ->
            DBNameStr = db_tools_dict:get_db_name(),
            FieldsStr = get_fields(TableInfo),
            IndexLStr = get_index_list(TableInfo),
            Options = get_options(TableInfo),
            {SQL, Args} = {"CREATE TABLE `~ts`.`~ts` (\n~ts", [DBNameStr, TableNameStr, FieldsStr]},
            {SQL1, Args1} = ?IF(IndexLStr =:= [], {SQL ++ "\n) ", Args}, {SQL ++ ",\n~ts\n) ", Args ++ [IndexLStr]}),
            {SQL2, Args2} = ?IF(Options =:= [], {SQL1 ++ ";", Args1}, {SQL1 ++ "~ts;", Args1 ++ [Options]}),
            SQL4 = io_lib:format(SQL2, Args2),
            execute(SQL4);
        false ->
            ok
    end.

is_not_table_exist(TableNameStr) ->
    DBNameStr = db_tools_dict:get_db_name(),
    SQL = io_lib:format("SELECT `TABLE_NAME` FROM `information_schema`.`TABLES` WHERE `TABLE_SCHEMA`='~ts' AND `TABLE_NAME`='~ts';",
        [DBNameStr, TableNameStr]),
    case query(SQL) of
        {ok, _Fields, [[TableNameStr | _]]} ->
            false;
        {ok, _Fields, _} ->
            true
    end.

%% 拼接表字段
get_fields(TableInfo) ->
    try
        Fields = db_tools_table:get_table_fields(TableInfo),
        lists:flatten(lists:join(",\n", [["\t", Str] || Str <- concat_fields(Fields)]))
    catch
        throw:Reason ->
            TableNameStr = db_tools_table:get_table_name(TableInfo),
            throw(<<"表："/utf8, TableNameStr/binary, " ", Reason/binary>>)
    end.

concat_fields([Field | T]) ->
    Result = concat_field(Field, [name, type, not_null, default, auto_inc, comment]),
    [lists:join(" ", Result) | concat_fields(T)];
concat_fields([]) ->
    [].

concat_field(ConfField, [name | T]) ->
    NameStr = db_tools_table:get_field_name(ConfField),
    [["`", NameStr, "`"] | concat_field(ConfField, T)];
concat_field(ConfField, [type | T]) ->
    Type = db_tools_table:get_field_type(ConfField),
    [[Type] | concat_field(ConfField, T)];
concat_field(ConfField, [not_null | T]) ->
    ?IF(?IS_DEFINED(not_null, ConfField), ["NOT NULL"], ["DEFAULT NULL"]) ++ concat_field(ConfField, T);
concat_field(ConfField, [default | T]) ->
    case ?GET_VALUE(default, ConfField) of
        undefined ->
            concat_field(ConfField, T);
        Default ->
            [["DEFAULT '", db_tools_util:any_to_list(Default), "'"] | concat_field(ConfField, T)]
    end;
concat_field(ConfField, [auto_inc | T]) ->
    ?IF(?IS_DEFINED(auto_inc, ConfField), ["AUTO_INCREMENT"], []) ++ concat_field(ConfField, T);
concat_field(ConfField, [comment | T]) ->
    case ?GET_VALUE(comment, ConfField) of
        undefined ->
            concat_field(ConfField, T);
        Comment ->
            [["COMMENT '", Comment, "'"] | concat_field(ConfField, T)]
    end;
concat_field(_FConfField, []) ->
    [].

%% 拼接表索引
get_index_list(TableInfo) ->
    try
        IndexList = db_tools_table:get_table_index_list(TableInfo),
        IndexList1 = [["\t", Str] || Str <- get_index_list_1(IndexList)],
        lists:flatten(lists:join(",\n", IndexList1))
    catch
        throw:Reason ->
            TableNameStr = db_tools_table:get_table_name(TableInfo),
            throw(<<"表："/utf8, TableNameStr/binary, " ", Reason/binary>>)
    end.

get_index_list_1([Index | T]) ->
    Result = concat_index(Index, [type, name, fields]),
    [lists:join(" ", Result) | get_index_list_1(T)];
get_index_list_1([]) ->
    [].

concat_index(Index, [type | T]) ->
    [?IF(?IS_DEFINED(primary, Index), ["PRIMARY KEY"],
        ?IF(?IS_DEFINED(unique, Index), ["UNIQUE KEY"], ["KEY"]))
        | concat_index(Index, T)];
concat_index(Index, [name | T]) ->
    IndexName = lists:join("_", [db_tools_util:any_to_list(Field) || Field <- ?GET_VALUE(fields, Index)]),
    [["`", IndexName, "`"] | concat_index(Index, T)];
concat_index(Index, [fields | T]) ->
    Fields = db_tools_table:get_index_fields(Index),
    FieldsStr = [["`", atom_to_list(Field), "`"] || Field <- Fields],
    [["(", lists:join(",", FieldsStr), ")"] | concat_index(Index, T)];
concat_index(_Index, []) ->
    [].

%% 拼接表选项
get_options(TableInfo) ->
    try
        TableOptions = db_tools_table:get_table_options(TableInfo),
        Result = concat_options(TableOptions, [engine, charset, collate, comment]),
        lists:flatten(lists:join(" ", Result))
    catch
        throw:Reason ->
            TableNameStr = db_tools_table:get_table_name(TableInfo),
            throw(<<"表："/utf8, TableNameStr/binary, " ", Reason/binary>>)
    end.

concat_options(TableOptions, [engine | T]) ->
    [["ENGINE=InnoDB DEFAULT"] | concat_options(TableOptions, T)];
concat_options(TableOptions, [charset | T]) ->
    case db_tools_dict:get_db_character() of
        undefined ->
            concat_options(TableOptions, T);
        Charset ->
            [["CHARSET=", Charset] | concat_options(TableOptions, T)]
    end;
concat_options(TableOptions, [collate | T]) ->
    case db_tools_dict:get_db_collation() of
        undefined ->
            concat_options(TableOptions, T);
        Collate ->
            [["COLLATE=", Collate] | concat_options(TableOptions, T)]
    end;
concat_options(TableOptions, [comment | T]) ->
    case db_tools_table:get_table_comment(TableOptions) of
        undefined ->
            concat_options(TableOptions, T);
        Comment ->
            [["COMMENT='", Comment, "'"] | concat_options(TableOptions, T)]
    end;
concat_options(_TableOptions, []) ->
    [].

%% 执行修改表结构
-spec do_alter_tables(Config :: list()) -> ok|no_return().
do_alter_tables(Config) ->
    Fun = fun(TableInfo) -> do_alter_table(TableInfo) end,
    lists:foreach(Fun, Config).

%% 检查修改表结构和索引
do_alter_table(TableInfo) ->
    %% 检查修改表结构
    DBFields = get_table_fields_desc(TableInfo),
    AlterFields = get_alter_fields(TableInfo, DBFields),
    lists:foreach(fun(SQL) -> execute(SQL) end, AlterFields),

    %% 检查修改表索引
    DBIndexList = get_table_index_desc(TableInfo),
    AlterIndexList = get_alter_index_list(TableInfo, DBIndexList),
    lists:foreach(fun(SQL) -> execute(SQL) end, AlterIndexList).

get_table_fields_desc(TableInfo) ->
    DBNameStr = db_tools_dict:get_db_name(),
    TableNameStr = db_tools_table:get_table_name(TableInfo),
    SQL = io_lib:format("SHOW FULL FIELDS FROM `~ts`.`~ts`;", [DBNameStr, TableNameStr]),
    {ok, _Fields, ValuesList} = query(SQL),
    get_table_fields_desc_1(ValuesList).

get_table_fields_desc_1([[Field, Type, Collection, Null, _Key, Default, Extra, _, Comment] | T]) ->
    Arg1 = [{name, db_tools_util:any_to_atom(Field)}],
    Arg2 = ?IF(Collection =/= null,
        [{type, db_tools_util:any_to_list(<<Type/binary, " COLLATE ", Collection/binary>>)}],
        [{type, db_tools_util:any_to_list(Type)}]),
    Arg3 = ?IF(Null =:= <<"NO">>, [not_null], []),
    Arg4 = ?IF(Default =/= null, [{default, db_tools_util:any_to_list(Default)}], []),
    Arg5 = ?IF(Extra =/= <<"">>, [auto_inc], []),
    Arg6 = ?IF(Comment =/= null, [{comment, unicode:characters_to_list(Comment)}], []),
    [Arg1 ++ Arg2 ++ Arg3 ++ Arg4 ++ Arg5 ++ Arg6 | get_table_fields_desc_1(T)];
get_table_fields_desc_1([]) ->
    [].

get_alter_fields(TableInfo, DBFields) ->
    DBNameStr = db_tools_dict:get_db_name(),
    TableNameStr = db_tools_table:get_table_name(TableInfo),
    ConfFields = db_tools_table:get_table_fields(TableInfo),
    [io_lib:format("ALTER TABLE `~ts`.`~ts` ~ts;", [DBNameStr, TableNameStr, FieldStr])
        || FieldStr <- concat_alter_fields(first, ConfFields, DBFields)].

concat_alter_fields(PrevConfField, [ConfField | T], DBFields) ->
    case compare_field(ConfField, DBFields) of
        {[], DBFields1} ->
            concat_alter_fields(ConfField, T, DBFields1);
        {FieldL, DBFields1} ->
            case PrevConfField of
                first ->
                    FieldStr = lists:flatten([FieldL, " FIRST "]);
                _ ->
                    FieldStr = lists:flatten([FieldL, " AFTER ", db_tools_table:get_field_name(PrevConfField)])
            end,
            [FieldStr | concat_alter_fields(ConfField, T, DBFields1)]
    end;
concat_alter_fields(_ConfField, [], DBFields) ->
    [["DROP COLUMN ", db_tools_table:get_field_name(DBField)] || DBField <- DBFields].

compare_field(ConfField, [DBField | T]) ->
    Ret = db_tools_util:run_fun_list([
        {fun compare_field_name/2, [ConfField, DBField]},
        {fun compare_field_type/2, [ConfField, DBField]},
        {fun compare_field_not_null/2, [ConfField, DBField]},
        {fun compare_field_auto_inc/2, [ConfField, DBField]},
        {fun compare_field_default/2, [ConfField, DBField]},
        {fun compare_field_comment/2, [ConfField, DBField]}
    ]),
    case Ret of
        true ->
            {[], T};
        field_name_difference ->
            {FieldList, DBFieldList} = compare_field(ConfField, T),
            {FieldList, [DBField | DBFieldList]};
        _ ->
            {["MODIFY COLUMN ", concat_fields([ConfField])], T}
    end;
compare_field(ConfField, []) ->
    {["ADD COLUMN ", concat_fields([ConfField]), ?IF(?IS_DEFINED(auto_inc, ConfField), "PRIMARY KEY", [])], []}.

compare_field_name(ConfField, DBField) ->
    ?IF(db_tools_table:get_field_name(ConfField) =:= db_tools_table:get_field_name(DBField),
        true, field_name_difference).

compare_field_type(ConfField, DBField) ->
    DBFieldTypes = string:split(db_tools_table:get_field_type(DBField), " "),
    case string:split(db_tools_table:get_field_type(ConfField), " ") of
        [ConfFieldType | []] ->
            %% 字符类型默认会增加`COLLATE`描述
            %% 例如：varchar(50) 实际为 varchar(50) COLLATE utf8_general_ci
            %% 当配置表仅有类型描述无其他额外内容时，只需匹配头部类型即可
            ?IF(ConfFieldType =:= hd(DBFieldTypes), true, field_type_difference);
        ConfFieldTypes ->
            ?IF(ConfFieldTypes =:= DBFieldTypes, true, field_type_difference)
    end.

compare_field_not_null(ConfField, DBField) ->
    ?IF(?IS_DEFINED(not_null, ConfField) =:= ?IS_DEFINED(not_null, DBField),
        true, field_not_null_difference).

compare_field_auto_inc(ConfField, DBField) ->
    ?IF(?IS_DEFINED(auto_inc, ConfField) =:= ?IS_DEFINED(auto_inc, DBField),
        true, field_auto_inc_difference).

compare_field_default(ConfField, DBField) ->
    ?IF(db_tools_util:any_to_list(?GET_VALUE(default, ConfField)) =:= ?GET_VALUE(default, DBField, "undefined"),
        true, field_default_difference).

compare_field_comment(ConfField, DBField) ->
    ?IF(?GET_VALUE(comment, ConfField, []) =:= ?GET_VALUE(comment, DBField, []),
        true, field_comment_difference).

get_table_index_desc(TableInfo) ->
    DBNameStr = db_tools_dict:get_db_name(),
    TableNameStr = db_tools_table:get_table_name(TableInfo),
    SQL = io_lib:format("SHOW INDEX FROM `~ts`.`~ts`;", [DBNameStr, TableNameStr]),
    {ok, _Fields, ValuesList} = query(SQL),
    lists:reverse(get_table_index_desc_1(ValuesList, [])).

get_table_index_desc_1([H | T], Result) ->
    [_, NonUnique, KeyName, Seq, ColumnName, _, _, _, _, _, _, _, _] = H,
    case Seq of
        1 ->
            Index = [
                {fields, [db_tools_util:any_to_atom(ColumnName)]},
                ?IF(KeyName =:= <<"PRIMARY">>, primary, ?IF(NonUnique =:= 0, unique, normal)),
                {name, db_tools_util:any_to_list(KeyName)}
            ],
            Result1 = [Index | Result];
        _ ->
            %% 联合索引，由于Index中有Atom数据类型，无法使用lists:keyxxx 相关操作，所以选择重新拼装一个Index结构
            [Index | OtherIndexList] = Result,
            Index1 = [
                {fields, ?GET_VALUE(fields, Index) ++ [db_tools_util:any_to_atom(ColumnName)]},
                ?IF(KeyName =:= <<"PRIMARY">>, primary, ?IF(NonUnique =:= 0, unique, normal)),
                {name, db_tools_util:any_to_list(KeyName)}
            ],
            Result1 = [Index1 | OtherIndexList]
    end,
    get_table_index_desc_1(T, Result1);
get_table_index_desc_1([], Result) ->
    Result.

get_alter_index_list(TableInfo, DBIndexList) ->
    DBNameStr = db_tools_dict:get_db_name(),
    TableNameStr = db_tools_table:get_table_name(TableInfo),
    ConfIndexList = db_tools_table:get_table_index_list(TableInfo),
    [io_lib:format("ALTER TABLE `~ts`.`~ts` ~ts;", [DBNameStr, TableNameStr, IndexStr]) ||
        IndexStr <- concat_alter_index(ConfIndexList, DBIndexList)].

concat_alter_index([ConfIndex | T], DBIndexList) ->
    {IndexList, DBIndexList1} = compare_index(ConfIndex, DBIndexList),
    concat_alter_index(T, DBIndexList1) ++ IndexList;
concat_alter_index([], DBIndexList) ->
    [
        ["DROP INDEX `", ?GET_VALUE(name, DBIndex), "`"]
        || DBIndex <- DBIndexList, not ?IS_DEFINED(primary, DBIndex)
    ].

compare_index(ConfIndex, [DBIndex | T]) ->
    Ret = db_tools_util:run_fun_list([
        {fun compare_index_fields/2, [ConfIndex, DBIndex]},
        {fun compare_index_type/2, [ConfIndex, DBIndex]}
    ]),
    case Ret of
        true ->
            {[], T};
        _ ->
            {IndexList, DBIndexList} = compare_index(ConfIndex, T),
            {IndexList, [DBIndex | DBIndexList]}
    end;
compare_index(ConfIndex, []) ->
    case ?IS_DEFINED(primary, ConfIndex) of
        true ->
            [Fields] = concat_index(ConfIndex, [fields]),
            {[["DROP PRIMARY KEY, ADD PRIMARY KEY ", Fields]], []};
        false ->
            [IndexName, Fields] = concat_index(ConfIndex, [name, fields]),
            case ?IS_DEFINED(unique, ConfIndex) of
                true ->
                    {[["ADD UNIQUE ", IndexName, " ", Fields]], []};
                false ->
                    {[["ADD INDEX ", IndexName, " ", Fields]], []}
            end
    end.

compare_index_fields(ConfIndex, DBIndex) ->
    ?IF(?GET_VALUE(fields, ConfIndex) -- ?GET_VALUE(fields, DBIndex) =:= [],
        true, index_fields_difference).

compare_index_type(ConfIndex, DBIndex) ->
    ConfIndexType = ?IF(?IS_DEFINED(primary, ConfIndex), primary, ?IF(?IS_DEFINED(unique, ConfIndex), unique, normal)),
    DBIndexType = ?IF(?IS_DEFINED(primary, DBIndex), primary, ?IF(?IS_DEFINED(unique, DBIndex), unique, normal)),
    ?IF(ConfIndexType =:= DBIndexType, true, index_type_difference).
