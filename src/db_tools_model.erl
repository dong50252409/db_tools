%%%-------------------------------------------------------------------
%%% @author gz1417
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 生成Erlang实体文件
%%% @end
%%% Created : 17. 8月 2021 14:34
%%%-------------------------------------------------------------------
-module(db_tools_model).

-include("db_tools.hrl").

%% API
-export([do_gen_model/1]).
%% 执行生成数据库表model
-spec do_gen_model(Config :: list()) -> ok.
do_gen_model(Config) ->
    Fun =
        fun(TableInfo) ->
            TableName = db_tools_table:get_table_name(TableInfo),
            TableComment = db_tools_table:get_table_comment(TableInfo),
            {TableFieldInfoList, ExtentFieldInfoList} = get_field_info_list(TableInfo),
            PrimaryKeyInfo = get_primary_key(TableInfo),
            ?VERBOSE("~ts 开始生成ERL...", [TableName]),
            db_tools_model_erl:do_gen_erl(TableName, TableComment, TableFieldInfoList, ExtentFieldInfoList, PrimaryKeyInfo),
            ?VERBOSE("~ts 开始生成HRL...", [TableName]),
            db_tools_model_hrl:do_gen_hrl(TableName, TableComment, TableFieldInfoList, ExtentFieldInfoList),
            ?VERBOSE("~ts 生成完毕！", [TableName])
        end,
    lists:foreach(Fun, Config).

get_field_info_list(TableInfo) ->
    Fields = db_tools_table:get_table_fields(TableInfo),
    ExtendFields = db_tools_table:get_table_extend_fields(TableInfo),
    {
        [get_field_info(Field, record_info(fields, field_info)) || Field <- Fields],
        [get_extend_field_info(Field, record_info(fields, field_info)) || Field <- ExtendFields]
    }.

get_field_info(Field, [name | T]) ->
    FieldInfo = get_field_info(Field, T),
    FieldInfo#field_info{name = db_tools_table:get_field_name(Field)};
get_field_info(Field, [type | T]) ->
    FieldInfo = get_field_info(Field, T),
    Type = db_tools_table:get_field_type(Field),
    TypeStr = unicode:characters_to_binary(string:trim(string:to_upper(Type))),
    FieldInfo#field_info{type = get_field_type(TypeStr)};
get_field_info(Field, [default | T]) ->
    FieldInfo = get_field_info(Field, T),
    case FieldInfo of
        #field_info{type = Type} when Type =:= "unicode:chardata()";Type =:= "binary()" ->
            FieldInfo#field_info{default = <<>>};
        #field_info{} ->
            FieldInfo#field_info{default = ?GET_VALUE(default, Field)}
    end;
get_field_info(Field, [comment | T]) ->
    FieldInfo = get_field_info(Field, T),
    case ?GET_VALUE(comment, Field) of
        undefined ->
            FieldInfo;
        Comment ->
            FieldInfo#field_info{comment = Comment}
    end;
get_field_info(Field, [to_term | T]) ->
    FieldInfo = get_field_info(Field, T),
    FieldInfo#field_info{to_term = ?IS_DEFINED(to_term, Field)};
get_field_info(_Field, []) ->
    #field_info{}.

get_field_type(<<"TINYINT", _/binary>>) ->
    "integer()";
get_field_type(<<"SMALLINT", _/binary>>) ->
    "integer()";
get_field_type(<<"MEDIUMINT", _/binary>>) ->
    "integer()";
get_field_type(<<"INT", _/binary>>) ->
    "integer()";
get_field_type(<<"INTEGER", _/binary>>) ->
    "integer()";
get_field_type(<<"BIGINT", _/binary>>) ->
    "integer()";
get_field_type(<<"FLOAT", _/binary>>) ->
    "float()";
get_field_type(<<"DOUBLE", _/binary>>) ->
    "float()";
get_field_type(<<"DECIMAL", _/binary>>) ->
    "float()";
get_field_type(<<"DATE", _/binary>>) ->
    "calendar:date()";
get_field_type(<<"TIME", _/binary>>) ->
    "calendar:time()";
get_field_type(<<"YEAR", _/binary>>) ->
    "calendar:year()";
get_field_type(<<"DATETIME", _/binary>>) ->
    "calendar:datetime()";
get_field_type(<<"TIMESTAMP", _/binary>>) ->
    "calendar:datetime";
get_field_type(<<"CHAR", _/binary>>) ->
    "unicode:chardata()";
get_field_type(<<"VARCHAR", _/binary>>) ->
    "unicode:chardata()";
get_field_type(<<"TEXT", _/binary>>) ->
    "unicode:chardata()";
get_field_type(<<"MEDIUMTEXT", _/binary>>) ->
    "unicode:chardata()";
get_field_type(<<"LONGTEXT", _/binary>>) ->
    "unicode:chardata()";
get_field_type(<<"TINYBLOB", _/binary>>) ->
    "binary()";
get_field_type(<<"BLOB", _/binary>>) ->
    "binary()";
get_field_type(<<"MEDIUMBLOB", _/binary>>) ->
    "binary()";
get_field_type(<<"LONGBLOB", _/binary>>) ->
    "binary()";
get_field_type(<<"BIT", _/binary>>) ->
    "binary()";
get_field_type(<<"JSON", _/binary>>) ->
    "jsx:json_term()";
get_field_type(_) ->
    "term()".

get_extend_field_info(Field, [name | T]) ->
    FieldInfo = get_extend_field_info(Field, T),
    FieldInfo#field_info{name = db_tools_table:get_field_name(Field)};
get_extend_field_info(Field, [type | T]) ->
    FieldInfo = get_extend_field_info(Field, T),
    TypeAtom = get_extend_field_type(?GET_VALUE(default, Field)),
    FieldInfo#field_info{type = TypeAtom};
get_extend_field_info(Field, [default | T]) ->
    FieldInfo = get_extend_field_info(Field, T),
    FieldInfo#field_info{default = ?GET_VALUE(default, Field)};
get_extend_field_info(Field, [comment | T]) ->
    FieldInfo = get_extend_field_info(Field, T),
    case ?GET_VALUE(comment, Field) of
        undefined ->
            FieldInfo;
        Comment ->
            FieldInfo#field_info{comment = Comment}
    end;
get_extend_field_info(Field, [to_term | T]) ->
    FieldInfo = get_field_info(Field, T),
    FieldInfo#field_info{to_term = ?IS_DEFINED(to_term, Field)};
get_extend_field_info(_Field, []) ->
    #field_info{}.

get_extend_field_type(undefined) ->
    "undefined";
get_extend_field_type(Term) when is_list(Term) ->
    "list()";
get_extend_field_type(Term) when is_tuple(Term) ->
    "tuple()";
get_extend_field_type(Term) when is_integer(Term) ->
    "integer()";
get_extend_field_type(Term) when is_binary(Term) ->
    "binary()";
get_extend_field_type(Term) when is_float(Term) ->
    "float()";
get_extend_field_type(Term) when is_boolean(Term) ->
    "boolean()";
get_extend_field_type(Term) when is_map(Term) ->
    "map()";
get_extend_field_type(Term) when is_atom(Term) ->
    "atom()".

get_primary_key(TableInfo) ->
    IndexList = db_tools_table:get_table_index_list(TableInfo),
    get_primary_key_1(IndexList).

get_primary_key_1([Index | T]) ->
    case ?IS_DEFINED(primary, Index) of
        true ->
            Fields = db_tools_table:get_index_fields(Index),
            #primary_key_info{field_list = [unicode:characters_to_binary(atom_to_list(Field)) || Field <- Fields]};
        false ->
            get_primary_key_1(T)
    end;
get_primary_key_1([]) ->
    #primary_key_info{}.