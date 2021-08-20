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

-record(field_info, {
    name,
    default,
    type,
    comment
}).

-record(primary_key_info, {
    field_list
}).

%% 执行生成数据库表model
-spec do_gen_model(Config :: list()) -> ok.
do_gen_model(Config) ->
    Fun =
        fun(TableInfo) ->
            TableName = db_tools_table:get_table_name(TableInfo),
            TableComment = db_tools_table:get_table_comment(TableInfo),
            FieldInfoList = get_field_info_list(TableInfo),
            PrimaryKeyInfo = get_primary_key(TableInfo),
            ?VERBOSE("~ts 开始生成ERL...", [TableName]),
            do_gen_erl(TableName, TableComment, FieldInfoList, PrimaryKeyInfo),
            ?VERBOSE("~ts 开始生成HRL...", [TableName]),
            do_gen_hrl(TableName, TableComment, FieldInfoList),
            ?VERBOSE("~ts 生成完毕！", [TableName])
        end,
    lists:foreach(Fun, Config).

get_field_info_list(TableInfo) ->
    Fields = db_tools_table:get_table_fields(TableInfo),
    ExtendFields = db_tools_table:get_table_extend_fields(TableInfo),
    [get_field_info(Field, record_info(fields, field_info)) || Field <- Fields]
    ++ [get_extend_field_info(Field, record_info(fields, field_info)) || Field <- ExtendFields].

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

do_gen_erl(TableName, TableComment, FieldInfoList, PrimaryKeyInfo) ->
    OutERL = db_tools_dict:get_out_erl(),
    ERLFilename = unicode:characters_to_binary([OutERL, db_tools_dict:get_erl_prefix(), TableName, ".erl"]),
    {ok, IO} = file:open(ERLFilename, [write, {encoding, utf8}]),
    gen_erl_head(TableName, TableComment, IO),
    gen_erl_new(TableName, FieldInfoList, IO),
    gen_erl_get_fields(FieldInfoList, IO),
    gen_erl_get_key_field_list(PrimaryKeyInfo, IO),
    gen_erl_get_key_index_list(FieldInfoList, PrimaryKeyInfo, IO).

gen_erl_head(TableName, TableComment, IO) ->
    io:format(IO, "%%%----------------------------------------------------\n", []),
    io:format(IO, "%%% AUTOMATIC GENERATION PLEASE DO NOT MODIFY\n", []),
    io:format(IO, "%%% 自动生成，请勿修改\n", []),
    io:format(IO, "%%% ~ts\n", [TableComment]),
    io:format(IO, "%%%----------------------------------------------------\n", []),
    ErlPrefix = db_tools_dict:get_erl_prefix(),
    io:format(IO, "-module(~ts~ts).\n\n", [ErlPrefix, TableName]),
    io:format(IO, "-include(\"~ts~ts.hrl\").\n\n", [ErlPrefix, TableName]),
    io:format(IO, "-compile(export_all).\n\n", []).

gen_erl_new(TableName, FieldInfoList, IO) ->
    io:format(IO, "-spec new_map() -> ~ts().\n", [TableName]),
    io:format(IO, "new_map() -> \n\t#{\n", []),
    gen_new_map_field(FieldInfoList, IO),
    io:format(IO, "\t}.\n\n", []),
    io:format(IO, "-spec new_record() -> #~ts{}.\n", [TableName]),
    io:format(IO, "new_record() -> \n\t#~ts{\n", [TableName]),
    gen_new_record_field(FieldInfoList, IO),
    io:format(IO, "\t}.\n\n", []).

gen_new_map_field([#field_info{name = Name, default = Default, comment = Comment} | T], IO) ->
    FieldStr = lists:flatten(io_lib:format(?IF(T =/= [], "\t\t~ts => ~w,", "\t\t~ts => ~w"), [Name, Default])),
    case Comment of
        undefined ->
            io:format(IO, "~ts\n", [FieldStr]);
        Comment ->
            io:format(IO, "~ts~ts% ~ts\n", [FieldStr, lists:duplicate(60 - length(FieldStr), " "), Comment])
    end,
    gen_new_map_field(T, IO);
gen_new_map_field([], _IO) ->
    ok.

gen_new_record_field([#field_info{name = Name, default = Default, comment = Comment} | T], IO) ->
    FieldStr = lists:flatten(io_lib:format(?IF(T =/= [], "\t\t~ts = ~w,", "\t\t~ts = ~w"), [Name, Default])),
    case Comment of
        undefined ->
            io:format(IO, "~ts\n", [FieldStr]);
        Comment ->
            io:format(IO, "~ts~ts% ~ts\n", [FieldStr, lists:duplicate(60 - length(FieldStr), " "), Comment])
    end,
    gen_new_record_field(T, IO);
gen_new_record_field([], _IO) ->
    ok.

gen_erl_get_fields(FieldInfoList, IO) ->
    io:format(IO, "-spec get_fields() -> list().\n", []),
    NameStrList = [Name || #field_info{name = Name} <- FieldInfoList],
    io:format(IO, "get_fields() -> \n\t[~ts].\n\n", [lists:join(", ", NameStrList)]).

gen_erl_get_key_field_list(#primary_key_info{field_list = FieldList}, IO) ->
    io:format(IO, "-spec get_key_field_list() -> list().\n", []),
    io:format(IO, "get_key_field_list() -> \n\t[~ts].\n\n", [lists:join(", ", FieldList)]).

gen_erl_get_key_index_list(FieldInfoList, #primary_key_info{field_list = FieldList}, IO) ->
    io:format(IO, "-spec get_key_index_list() -> list().\n", []),
    io:format(IO, "get_key_index_list() -> \n\t[~ts].\n\n", [lists:join(", ", get_key_index(FieldInfoList, FieldList, 2))]).

get_key_index([#field_info{name = Name} | T1], [Name | T2], Index) ->
    [integer_to_list(Index) | get_key_index(T1, T2, Index + 1)];
get_key_index([_ | T], FieldList, Index) ->
    get_key_index(T, FieldList, Index);
get_key_index([], [], _Index) ->
    [].

do_gen_hrl(TableName, TableComment, FieldInfoList) ->
    OutHRL = db_tools_dict:get_out_hrl(),
    HRLFilename = unicode:characters_to_binary([OutHRL, db_tools_dict:get_hrl_prefix(), TableName, ".hrl"]),
    {ok, IO} = file:open(HRLFilename, [write, {encoding, utf8}]),
    gen_hrl_head(TableName, TableComment, IO),
    gen_hrl_spec(TableName, FieldInfoList, IO),
    gen_hrl_record(TableName, FieldInfoList, IO),
    gen_hrl_tail(IO).

gen_hrl_head(TableName, TableComment, IO) ->
    io:format(IO, "%%%----------------------------------------------------\n", []),
    io:format(IO, "%%% AUTOMATIC GENERATION PLEASE DO NOT MODIFY\n", []),
    io:format(IO, "%%% 自动生成，请勿修改\n", []),
    io:format(IO, "%%% ~ts\n", [TableComment]),
    io:format(IO, "%%%----------------------------------------------------\n", []),
    HRLPrefix = string:to_upper(db_tools_dict:get_hrl_prefix()),
    TableName1 = string:to_upper(db_tools_util:any_to_list(TableName)),
    io:format(IO, "-ifndef(~ts~ts_HRL).\n", [HRLPrefix, TableName1]),
    io:format(IO, "-define(~ts~ts_HRL, true).\n\n", [HRLPrefix, TableName1]),
    io:format(IO, "-define(~ts~ts, ~ts).\n\n", [HRLPrefix, TableName1, TableName]).

gen_hrl_spec(TableName, FieldInfoList, IO) ->
    io:format(IO, "-export_type([~ts/0]).\n\n", [TableName]),
    io:format(IO, "-type ~ts() :: #{\n", [TableName]),
    gen_hrl_spec_field(FieldInfoList, IO),
    io:format(IO, "}.\n\n", []).

gen_hrl_spec_field([#field_info{name = Name, type = Type, comment = Comment} | T], IO) ->
    FieldStr = lists:flatten(io_lib:format(?IF(T =/= [], "\t\t~ts := ~ts,", "\t\t~ts := ~ts"), [Name, Type])),
    case Comment of
        undefined ->
            io:format(IO, "~ts\n", [FieldStr]);
        Comment ->
            io:format(IO, "~ts~ts% ~ts\n", [FieldStr, lists:duplicate(60 - length(FieldStr), " "), Comment])
    end,
    gen_hrl_spec_field(T, IO);
gen_hrl_spec_field([], _IO) ->
    ok.

gen_hrl_record(TableName, FieldInfoList, IO) ->
    io:format(IO, "-record(~ts, {\n", [TableName]),
    gen_hrl_record_field(FieldInfoList, IO),
    io:format(IO, "}).\n\n", []).

gen_hrl_record_field([#field_info{name = Name, default = Default, type = Type, comment = Comment} | T], IO) ->
    case Default of
        undefined ->
            FieldStr = lists:flatten(io_lib:format(?IF(T =/= [], "\t\t~ts :: ~ts,", "\t\t~ts :: ~ts"), [Name, Type]));
        Default ->
            FieldStr = lists:flatten(io_lib:format(?IF(T =/= [], "\t\t~ts = ~w :: ~ts,", "\t\t~ts = ~w :: ~ts"), [Name, Default, Type]))
    end,
    case Comment of
        undefined ->
            io:format(IO, "~ts\n", [FieldStr]);
        Comment ->
            io:format(IO, "~ts~ts% ~ts\n", [FieldStr, lists:duplicate(60 - length(FieldStr), " "), Comment])
    end,
    gen_hrl_record_field(T, IO);
gen_hrl_record_field([], _IO) ->
    ok.

gen_hrl_tail(IO) ->
    io:format(IO, "-endif.", []).