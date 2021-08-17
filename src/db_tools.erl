-module(db_tools).

-include("db_tools.hrl").

%% API exports
-export([main/1]).

%% Internal API
-export([do_export_db/1]).

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
            try
                Config = get_filename(),
                db_tools_operation:do_connect_db(),
                db_tools_operation:do_create_db(),
                db_tools_operation:do_create_tables(Config),
                db_tools_operation:do_alter_tables(Config)
            catch
                throw:Reason ->
                    ?CONSOLE("执行失败:~tp", [Reason]);
                Err:Reason:Track ->
                    ?CONSOLE("Err:~w Reason:~w\nTrack:~tp", [Err, Reason, Track])
            end;
        help ->
            db_tools_helper:help()
    end,
    do_close_export().

get_filename() ->
    Filename = db_tools_dict:get_config_filename(),
    {ok, [Config]} = file:consult(Filename),
    Config.

-spec do_export_db(SQL :: string()) -> ok|{error, Reason :: file:posix() | badarg | terminated}.
do_export_db(SQL) ->
    case db_tools_dict:get_export_filename() of
        undefined ->
            ok;
        IO when is_pid(IO) ->
            io:format(IO, "~ts\n\n", [SQL])
    end.

do_close_export() ->
    case db_tools_dict:get_export_filename() of
        undefined ->
            ok;
        IO when is_pid(IO) ->
            file:close(IO)
    end.