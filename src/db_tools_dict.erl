%%%-------------------------------------------------------------------
%%% @author gz1417
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. 8月 2021 17:27
%%%-------------------------------------------------------------------
-module(db_tools_dict).

-include("db_tools.hrl").

%% API
-export([
    set_verbose/0, is_verbose/0,
    set_db_character/1, get_db_character/0,
    set_db_collation/1, get_db_collation/0,
    set_config_filename/1, get_config_filename/0,
    set_db_host/1, get_db_host/0,
    set_db_port/1, get_db_port/0,
    set_db_user/1, get_db_user/0,
    set_db_passwd/1, get_db_passwd/0,
    set_db_name/1, get_db_name/0,
    set_export_filename/1, get_export_filename/0,
    set_db_conn/1, get_db_conn/0
]).

set_verbose() ->
    put(verbose, true).

is_verbose() ->
    get(verbose) =:= true.

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


set_config_filename(Filename) ->
    put(config_filename, Filename),
    ok.

get_config_filename() ->
    ?CHECK(get(config_filename), <<"请指定配置表文件"/utf8>>).

set_db_host(Host) ->
    put(db_host, Host).

get_db_host() ->
    case get(db_host) of
        undefined ->
            ?DEFAULT_HOST;
        Value ->
            Value
    end.

set_db_port(Port) ->
    put(db_port, Port).

get_db_port() ->
    case get(db_port) of
        undefined ->
            ?DEFAULT_PORT;
        Value ->
            Value
    end.

set_db_user(User) ->
    put(db_user, User).

get_db_user() ->
    ?CHECK(get(db_user), <<"请定指定用户名"/utf8>>).

set_db_passwd(Passwd) ->
    put(db_passwd, Passwd).

get_db_passwd() ->
    ?CHECK(get(db_passwd), <<"请指定密码"/utf8>>).

set_db_name(DBName) when is_list(DBName) ->
    put(db_name, DBName);
set_db_name(Config) when is_tuple(Config) ->
    get_db_name() =:= undefined andalso put(db_name, unicode:characters_to_binary(atom_to_binary(element(1, Config)))).

get_db_name() ->
    get(db_name).

set_export_filename(Filename) ->
    {ok, IO} = file:open(Filename, [write]),
    put(export_filename, IO).

get_export_filename() ->
    get(export_filename).

set_db_conn(Conn) ->
    put(db_conn, Conn).

get_db_conn() ->
    get(db_conn).
