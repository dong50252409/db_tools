%%%-------------------------------------------------------------------
%%% @author gz1417
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 生成 model Erlang文件
%%% @end
%%% Created : 03. 9月 2021 11:10
%%%-------------------------------------------------------------------
-module(db_tools_model_erl).

-include("db_tools.hrl").
%% API
-export([do_gen_erl/5]).

do_gen_erl(TableName, TableComment, TableFieldInfoList, ExtentFieldInfoList, PrimaryKeyInfo) ->
    OutERL = db_tools_dict:get_out_erl(),
    ERLFilename = unicode:characters_to_binary([OutERL, db_tools_dict:get_erl_prefix(), TableName, ".erl"]),
    {ok, IO} = file:open(ERLFilename, [write, {encoding, utf8}]),
    FieldInfoList = TableFieldInfoList ++ ExtentFieldInfoList,
    gen_head(TableName, TableComment, IO),
    gen_get_table_name(TableName, IO),
    gen_new_map(TableName, FieldInfoList, IO),
    gen_new_record(TableName, FieldInfoList, IO),
    gen_as_map(TableName, TableFieldInfoList, IO),
    gen_as_record(TableName, TableFieldInfoList, IO),
    gen_get_table_key_field_list(PrimaryKeyInfo, IO),
    gen_get_table_key_index_list(PrimaryKeyInfo, IO),
    gen_get_table_field_list(TableFieldInfoList, IO),
    gen_get_table_index_list(TableName, TableFieldInfoList, IO),
    gen_get_table_values(TableName, TableFieldInfoList, IO).

gen_head(TableName, TableComment, IO) ->
    io:format(IO, "%%%----------------------------------------------------~n", []),
    io:format(IO, "%%% AUTOMATIC GENERATION PLEASE DO NOT MODIFY~n", []),
    io:format(IO, "%%% 自动生成，请勿修改~n", []),
    io:format(IO, "%%% ~ts~n", [TableComment]),
    io:format(IO, "%%%----------------------------------------------------~n", []),
    ErlPrefix = db_tools_dict:get_erl_prefix(),
    io:format(IO, "-module(~ts~ts).~n~n", [ErlPrefix, TableName]),
    io:format(IO, "-include(\"~ts~ts.hrl\").~n~n", [ErlPrefix, TableName]),
    io:format(IO, "-compile(export_all).~n~n", []).

gen_get_table_name(TableName, IO) ->
    io:format(IO, "-spec get_table_name() -> atom().~n", []),
    io:format(IO, "get_table_name() -> ~n    ~ts.~n~n", [TableName]).

gen_new_map(TableName, FieldInfoList, IO) ->
    io:format(IO, "-spec new_map() -> ~ts().~n", [TableName]),
    io:format(IO, "new_map() -> ~n    #{~n", []),
    gen_new_map_body(FieldInfoList, IO),
    io:format(IO, "    }.~n~n", []).

gen_new_map_body([#field_info{name = Name, default = Default, comment = Comment} | T], IO) ->
    FieldStr = lists:flatten(io_lib:format(?IF(T =/= [], "        ~ts => ~w,", "        ~ts => ~w"), [Name, Default])),
    case Comment of
        undefined ->
            io:format(IO, "~ts~n", [FieldStr]);
        Comment ->
            io:format(IO, "~ts~ts% ~ts~n", [FieldStr, lists:duplicate(64 - length(FieldStr), " "), Comment])
    end,
    gen_new_map_body(T, IO);
gen_new_map_body([], _IO) ->
    ok.

gen_new_record(TableName, FieldInfoList, IO) ->
    io:format(IO, "-spec new_record() -> #~ts{}.~n", [TableName]),
    io:format(IO, "new_record() -> ~n    #~ts{~n", [TableName]),
    gen_new_record_body(FieldInfoList, IO),
    io:format(IO, "    }.~n~n", []).

gen_new_record_body([#field_info{name = Name, default = Default, comment = Comment} | T], IO) ->
    FieldStr = lists:flatten(io_lib:format(?IF(T =/= [], "        ~ts = ~w,", "        ~ts = ~w"), [Name, Default])),
    case Comment of
        undefined ->
            io:format(IO, "~ts~n", [FieldStr]);
        Comment ->
            io:format(IO, "~ts~ts% ~ts~n", [FieldStr, lists:duplicate(64 - length(FieldStr), " "), Comment])
    end,
    gen_new_record_body(T, IO);
gen_new_record_body([], _IO) ->
    ok.

gen_as_map(TableName, TableFieldInfoList, IO) ->
    io:format(IO, "-spec as_map(list()) -> ~ts().~n", [TableName]),
    FuncName = io_lib:format("as_map([~ts]) ->", [gen_as_head(1, length(TableFieldInfoList))]),
    io:format(IO, "~ts~n", [pretty_print(FuncName, 4)]),
    io:format(IO, "    Map = new_map(),~n", []),
    Body = gen_as_map_body(TableFieldInfoList, 1),
    io:format(IO, "    Map#{~n        ~ts~n    }.~n~n", [pretty_print(Body, 8)]),
    ok.

gen_as_head(Max, Max) ->
    [io_lib:format("V~w", [Max])];
gen_as_head(N, Max) ->
    [io_lib:format("V~w, ", [N]) | gen_as_head(N + 1, Max)].

gen_as_map_body([#field_info{name = Name, type = Type, to_term = ToTerm} | T], N) ->
    Str1 = io_lib:format("~ts := ", [Name]),
    case {Type, ToTerm} of
        {"unicode:chardata()", true} ->
            Str2 = io_lib:format("db_util:string_to_term(V~w)", [N]);
        {"binary()", true} ->
            Str2 = io_lib:format("db_util:string_to_term(V~w)", [N]);
        {"jsx:json_term()", true} ->
            Str2 = io_lib:format("db_util:json_to_term(V~w)", [N]);
        _ ->
            Str2 = io_lib:format("V~w", [N])
    end,
    case T of
        [] ->
            [Str1, Str2];
        _ ->
            [Str1, Str2, io_lib:format(", ", []) | gen_as_map_body(T, N + 1)]
    end.

gen_as_record(TableName, TableFieldInfoList, IO) ->
    io:format(IO, "-spec as_record(list()) -> #~ts{}.~n", [TableName]),
    FuncName = io_lib:format("as_record([~ts]) ->", [gen_as_head(1, length(TableFieldInfoList))]),
    io:format(IO, "~ts~n", [pretty_print(FuncName, 4)]),
    io:format(IO, "    Record = new_record(),~n", []),
    Body = gen_as_record_body(TableFieldInfoList, 1),
    io:format(IO, "    Record#~ts{~n        ~ts~n    }.~n~n", [TableName, pretty_print(Body, 8)]),
    ok.

gen_as_record_body([#field_info{name = Name, type = Type, to_term = ToTerm} | T], N) ->
    Str1 = io_lib:format("~ts = ", [Name]),
    case {Type, ToTerm} of
        {"unicode:chardata()", true} ->
            Str2 = io_lib:format("db_util:string_to_term(V~w)", [N]);
        {"binary()", true} ->
            Str2 = io_lib:format("db_util:string_to_term(V~w)", [N]);
        {"jsx:json_term()", true} ->
            Str2 = io_lib:format("db_util:json_to_term(V~w)", [N]);
        _ ->
            Str2 = io_lib:format("V~w", [N])
    end,
    case T of
        [] ->
            [Str1, Str2];
        _ ->
            [Str1, Str2, io_lib:format(", ", []) | gen_as_record_body(T, N + 1)]
    end.

gen_get_table_key_field_list(PrimaryKeyInfo, IO) ->
    io:format(IO, "-spec get_table_key_filed_list() -> list().~n", []),
    FieldList = PrimaryKeyInfo#primary_key_info.field_list,
    io:format(IO, "get_table_key_filed_list() ->~n    [~ts].~n~n", [lists:join(", ", FieldList)]).

gen_get_table_key_index_list(PrimaryKeyInfo, IO) ->
    io:format(IO, "-spec get_table_key_index_list() -> list().~n", []),
    IndexList = PrimaryKeyInfo#primary_key_info.index_list,
    io:format(IO, "get_table_key_index_list() ->~n    [~ts].~n~n", [lists:join(", ", IndexList)]).

gen_get_table_field_list(TableFieldInfoList, IO) ->
    io:format(IO, "-spec get_table_field_list() -> list().~n", []),
    NameStrList = [Name || #field_info{name = Name} <- TableFieldInfoList],
    Body = io_lib:format("[~ts]", [lists:join(", ", NameStrList)]),
    io:format(IO, "get_table_field_list() ->~n    ~ts.~n~n", [pretty_print(Body, 8)]).

gen_get_table_index_list(TableName, TableFieldInfoList, IO) ->
    io:format(IO, "-spec get_table_index_list() -> list().~n", []),
    NameStrList = [["#", TableName, ".", Name] || #field_info{name = Name} <- TableFieldInfoList],
    Body = io_lib:format("[~ts]", [lists:join(", ", NameStrList)]),
    io:format(IO, "get_table_index_list() ->~n    ~ts.~n~n", [pretty_print(Body, 8)]).

gen_get_table_values(TableName, TableFieldInfoList, IO) ->
    io:format(IO, "-spec get_table_values(~ts()|#~ts{}) -> list().~n", [TableName, TableName]),
    FuncName1 = io_lib:format("get_table_values(#{~ts}) ->", [gen_get_table_values_head1(TableFieldInfoList, 1)]),
    io:format(IO, "~ts~n", [pretty_print(FuncName1, 4)]),
    Body = pretty_print(io_lib:format("[~ts]", [gen_get_table_values_body(TableFieldInfoList, 1)]), 8),
    io:format(IO, "    ~ts;~n", [Body]),

    FuncName2 = io_lib:format("get_table_values(#~ts{~ts}) -> ", [TableName, gen_get_table_values_head2(TableFieldInfoList, 1)]),
    io:format(IO, "~ts~n", [pretty_print(FuncName2, 4)]),
    io:format(IO, "    ~ts.~n", [Body]),
    ok.

gen_get_table_values_head1([#field_info{name = Name} | T], N) ->
    case T of
        [] ->
            [io_lib:format("~ts := V~w", [Name, N])];
        T ->
            [io_lib:format("~ts := V~w, ", [Name, N]) | gen_get_table_values_head1(T, N + 1)]
    end.

gen_get_table_values_head2([#field_info{name = Name} | T], N) ->
    case T of
        [] ->
            [io_lib:format("~ts = V~w", [Name, N])];
        T ->
            [io_lib:format("~ts = V~w, ", [Name, N]) | gen_get_table_values_head2(T, N + 1)]
    end.

gen_get_table_values_body([#field_info{type = Type, to_term = ToTerm} | T], N) ->
    case {Type, ToTerm} of
        {"unicode:chardata()", true} ->
            Str = io_lib:format("db_util:term_to_binary(V~w)", [N]);
        {"binary()", true} ->
            Str = io_lib:format("db_util:term_to_binary(V~w)", [N]);
        {"jsx:json_term()", true} ->
            Str = io_lib:format("db_util:term_to_json(V~w)", [N]);
        _ ->
            Str = io_lib:format("V~w", [N])
    end,
    case T of
        [] ->
            [Str];
        T ->
            [Str, io_lib:format(", ", []) | gen_get_table_values_body(T, N + 1)]
    end.


pretty_print(Str, SpecNum) ->
    Spec = lists:duplicate(SpecNum, " "),
    pp_1(lists:flatten(Str), Spec, 1).
pp_1([S1, S2 | T], Spec, Count) when Count >= 60, S1 =:= $,, S2 =:= $  ->
    [S1, "\n", Spec | pp_1(T, Spec, 1)];
pp_1([S | T], Spec, Count) when Count >= 60, S =:= $, ->
    [S, "\n", Spec | pp_1(T, Spec, 1)];
pp_1([S | T], Spec, Count) ->
    [S | pp_1(T, Spec, Count + 1)];
pp_1([], _Spec, _Count) ->
    [].
