%%%-------------------------------------------------------------------
%%% @author gz1417
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 生成model头文件
%%% @end
%%% Created : 03. 9月 2021 11:09
%%%-------------------------------------------------------------------
-module(db_tools_model_hrl).

-include("db_tools.hrl").

%% API
-export([do_gen_hrl/4]).

do_gen_hrl(TableName, TableComment, TableFieldInfoList, ExtentFieldInfoList) ->
    FieldInfoList = TableFieldInfoList ++ ExtentFieldInfoList,
    OutHRL = db_tools_dict:get_out_hrl(),
    HRLFilename = unicode:characters_to_binary([OutHRL, db_tools_dict:get_hrl_prefix(), TableName, ".hrl"]),
    {ok, IO} = file:open(HRLFilename, [write, {encoding, utf8}]),
    gen_head(TableName, TableComment, IO),
    gen_spec(TableName, FieldInfoList, IO),
    gen_record(TableName, FieldInfoList, IO),
    gen_tail(IO).

gen_head(TableName, TableComment, IO) ->
    io:format(IO, "%%%----------------------------------------------------~n", []),
    io:format(IO, "%%% AUTOMATIC GENERATION PLEASE DO NOT MODIFY~n", []),
    io:format(IO, "%%% 自动生成，请勿修改~n", []),
    io:format(IO, "%%% ~ts~n", [TableComment]),
    io:format(IO, "%%%----------------------------------------------------~n", []),
    HRLPrefix = string:to_upper(db_tools_dict:get_hrl_prefix()),
    TableName1 = string:to_upper(db_tools_util:any_to_list(TableName)),
    io:format(IO, "-ifndef(~ts~ts_HRL).~n", [HRLPrefix, TableName1]),
    io:format(IO, "-define(~ts~ts_HRL, true).~n~n", [HRLPrefix, TableName1]),
    io:format(IO, "-define(~ts~ts, ~ts).~n~n", [HRLPrefix, TableName1, TableName]).

gen_spec(TableName, FieldInfoList, IO) ->
    io:format(IO, "-export_type([~ts/0]).~n~n", [TableName]),
    io:format(IO, "-type ~ts() :: #{~n", [TableName]),
    gen_spec_field(FieldInfoList, IO),
    io:format(IO, "}.~n~n", []).

gen_spec_field([#field_info{name = Name, type = Type, comment = Comment} | T], IO) ->
    FieldStr = lists:flatten(io_lib:format(?IF(T =/= [], "        ~ts := ~ts,", "        ~ts := ~ts"), [Name, Type])),
    case Comment of
        undefined ->
            io:format(IO, "~ts~n", [FieldStr]);
        Comment ->
            io:format(IO, "~ts~ts% ~ts~n", [FieldStr, lists:duplicate(60 - length(FieldStr), " "), Comment])
    end,
    gen_spec_field(T, IO);
gen_spec_field([], _IO) ->
    ok.

gen_record(TableName, FieldInfoList, IO) ->
    io:format(IO, "-record(~ts, {~n", [TableName]),
    gen_record_field(FieldInfoList, IO),
    io:format(IO, "}).~n~n", []).

gen_record_field([#field_info{name = Name, default = Default, type = Type, comment = Comment} | T], IO) ->
    case Default of
        undefined ->
            FieldStr = lists:flatten(io_lib:format(?IF(T =/= [], "        ~ts :: ~ts,", "        ~ts :: ~ts"), [Name, Type]));
        Default ->
            FieldStr = lists:flatten(io_lib:format(?IF(T =/= [], "        ~ts = ~w :: ~ts,", "        ~ts = ~w :: ~ts"), [Name, Default, Type]))
    end,
    case Comment of
        undefined ->
            io:format(IO, "~ts~n", [FieldStr]);
        Comment ->
            io:format(IO, "~ts~ts% ~ts~n", [FieldStr, lists:duplicate(60 - length(FieldStr), " "), Comment])
    end,
    gen_record_field(T, IO);
gen_record_field([], _IO) ->
    ok.

gen_tail(IO) ->
    io:format(IO, "-endif.", []).