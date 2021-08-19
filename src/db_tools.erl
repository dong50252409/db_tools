-module(db_tools).

-include("db_tools.hrl").

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================
%% escript Entry point
main(Args) ->
    case os:type() of
        {win32, nt} ->
            os:cmd("chcp 65001");
        _ ->
            ok
    end,
    do_main(Args),
    erlang:halt(0).
%%====================================================================
%% Internal functions
%%====================================================================
do_main([]) ->
    db_tools_helper:help();
do_main(Args) ->
    case db_tools_helper:parse_args(Args) of
        ok ->
            do(db_tools_dict:get_mode());
        help ->
            db_tools_helper:help()
    end.

do(?MODE_UPDATE_DB) ->
    try
        Config = get_filename(),
        db_tools_db_operation:do_connect_db(),
        db_tools_db_operation:do_create_db(),
        db_tools_db_operation:do_create_tables(Config),
        db_tools_db_operation:do_alter_tables(Config)
    catch
        throw:Reason ->
            ?CONSOLE("更新创建MySQL数据库表结构失败，原因：~ts", [Reason]);
        Err:Reason:Track ->
            ?CONSOLE("Err:~w Reason:~w\nTrack:~tp", [Err, Reason, Track])
    after
        db_tools_db_operation:do_close_io()
    end;

do(?MODE_TRUNCATE_DB) ->
    ok;

do(?MODE_GEN_ENTITY) ->
    try
        Config = get_filename(),
        db_tools_gen_entity:do_gen_entity(Config)
    catch
        throw:Reason ->
            ?CONSOLE("生成Erlang实体文件失败，原因：~ts", [Reason]);
        Err:Reason:Track ->
            ?CONSOLE("Err:~w Reason:~w\nTrack:~tp", [Err, Reason, Track])
    end .

get_filename() ->
    Filename = db_tools_dict:get_config_filename(),
    {ok, [Config]} = file:consult(Filename),
    Config.

