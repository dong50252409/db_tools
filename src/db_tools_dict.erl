%%%-------------------------------------------------------------------
%%% @author gz1417
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 进程字典封装
%%% @end
%%% Created : 02. 8月 2021 17:27
%%%-------------------------------------------------------------------
-module(db_tools_dict).

-include("db_tools.hrl").

%% API
-export([
    set_verbose/0, is_verbose/0,
    set_config_filename/1, get_config_filename/0,
    set_mode/1, get_mode/0,
    set_db_character/1, get_db_character/0,
    set_db_collation/1, get_db_collation/0,
    set_db_host/1, get_db_host/0,
    set_db_port/1, get_db_port/0,
    set_db_user/1, get_db_user/0,
    set_db_passwd/1, get_db_passwd/0,
    set_db_name/1, get_db_name/0,
    set_export_filename/1, get_export_io/0,
    set_not_del_tbl/0, is_not_del_tbl/0,
    set_not_del_field/0, is_not_del_field/0,
    set_out_hrl/1, get_out_hrl/0,
    set_hrl_prefix/1, get_hrl_prefix/0,
    set_out_erl/1, get_out_erl/0,
    set_erl_prefix/1, get_erl_prefix/0,
    set_db_conn/1, get_db_conn/0
]).

set_verbose() ->
    put(verbose, true),
    ok.

is_verbose() ->
    get(verbose) =:= true.

set_config_filename(Filename) ->
    put(config_filename, Filename),
    ok.

get_config_filename() ->
    ?CHECK(get(config_filename), <<"请指定配置表文件"/utf8>>).

set_mode(ModeStr) ->
    case ModeStr of
        "update_db" ->
            Mode = ?MODE_UPDATE_DB,
            put(mode, Mode);
        "truncate_db" ->
            Mode = ?MODE_TRUNCATE_DB,
            put(mode, Mode);
        "gen_model" ->
            Mode = ?MODE_GEN_MODEL,
            put(mode, Mode);
        _ ->
            throw(<<"模式不存在"/utf8>>)
    end,
    ok.

get_mode() ->
    ?CHECK(get(mode), <<"请指定模式"/utf8>>).

set_db_character(DBCharacter) ->
    put(db_character, DBCharacter),
    ok.

get_db_character() ->
    get(db_character).

set_db_collation(DBCollation) ->
    put(db_collation, DBCollation),
    ok.

get_db_collation() ->
    get(db_collation).

set_db_host(Host) ->
    put(db_host, Host),
    ok.

get_db_host() ->
    case get(db_host) of
        undefined ->
            ?DEFAULT_HOST;
        Value ->
            Value
    end.

set_db_port(Port) ->
    put(db_port, Port),
    ok.

get_db_port() ->
    case get(db_port) of
        undefined ->
            ?DEFAULT_PORT;
        Value ->
            Value
    end.

set_db_user(User) ->
    put(db_user, User),
    ok.

get_db_user() ->
    ?CHECK(get(db_user), <<"请定指定用户名"/utf8>>).

set_db_passwd(Passwd) ->
    put(db_passwd, Passwd),
    ok.

get_db_passwd() ->
    ?CHECK(get(db_passwd), <<"请指定密码"/utf8>>).

set_db_name(DBName) when is_list(DBName) ->
    put(db_name, unicode:characters_to_binary(DBName)),
    ok.

get_db_name() ->
    ?CHECK(get(db_name), <<"请指定数据库名"/utf8>>).

set_export_filename(Filename) ->
    filelib:ensure_dir(Filename),
    {ok, IO} = file:open(Filename, [write, {encoding, utf8}]),
    put(export_filename, IO),
    ok.

set_not_del_tbl() ->
    put(not_del_tbl, true),
    ok.

is_not_del_tbl() ->
    ?IF(get(not_del_tbl) =:= true, true, false).

set_not_del_field() ->
    put(not_del_field, true),
    ok.

is_not_del_field() ->
    ?IF(get(not_del_field) =:= true, true, false).

get_export_io() ->
    get(export_filename).

set_out_hrl(OutHRL) ->
    case lists:last(OutHRL) of
        "/" ->
            OutHRL1 = OutHRL;
        _ ->
            OutHRL1 = OutHRL ++ "/"
    end,
    filelib:ensure_dir(OutHRL1),
    put(out_hrl, OutHRL1),
    ok.

get_out_hrl() ->
    case get(out_hrl) of
        undefined ->
            "";
        OutHRL ->
            OutHRL
    end.

set_hrl_prefix(HRLPrefix) ->
    put(hrl_prefix, HRLPrefix),
    ok.

get_hrl_prefix() ->
    case get(hrl_prefix) of
        undefined ->
            "";
        HRLPrefix ->
            HRLPrefix
    end.

set_out_erl(OutERL) ->
    case lists:last(OutERL) of
        "/" ->
            OutERL1 = OutERL;
        _ ->
            OutERL1 = OutERL ++ "/"
    end,
    filelib:ensure_dir(OutERL1),
    put(out_erl, OutERL1),
    ok.

get_out_erl() ->
    case get(out_erl) of
        undefined ->
            "";
        OutERL ->
            OutERL
    end.

set_erl_prefix(ERLPrefix) ->
    put(erl_prefix, ERLPrefix),
    ok.

get_erl_prefix() ->
    case get(erl_prefix) of
        undefined ->
            "";
        ERLPrefix ->
            ERLPrefix
    end.

set_db_conn(Conn) ->
    put(db_conn, Conn),
    ok.

get_db_conn() ->
    get(db_conn).
