%% -*- erlang -*-
%%! +pc unicode
-module(db_tools).

-include("db_tools.hrl").

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================
%% escript Entry point
main(Args) ->
    try
        case lists:member({encoding, latin1}, io:getopts()) andalso os:type() =:= {win32, nt} of
            true ->
                os:cmd("chcp 65001");
            false ->
                ok
        end,
        do_main(Args)
    catch
        throw:Reason ->
            ?CONSOLE("操作执行失败！原因：~tp", [Reason]);
        Err:Reason:Track ->
            ?CONSOLE("Err:~w Reason:~w\nTrack:~tp", [Err, Reason, Track])
    end,
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
    Config = get_filename(),
    db_tools_operation:do_update_db(Config);

do(?MODE_TRUNCATE_DB) ->
    Config = get_filename(),
    db_tools_operation:do_truncate_tables(Config);

do(?MODE_GEN_MODEL) ->
    Config = get_filename(),
    db_tools_model:do_gen_model(Config);

do(?MODE_GEN_MODEL_BY_DB) ->
    Config = db_tools_operation:do_collect_datatables_config(),
    db_tools_model:do_gen_model(Config).

get_filename() ->
    Filename = db_tools_dict:get_config_filename(),
    {ok, [Config]} = file:consult(Filename),
    Config.

