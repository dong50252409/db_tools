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
main([]) ->
    db_tools_helper:help(),
    erlang:halt(0);
main(Args) ->
    case db_tools_helper:parse_args(Args) of
        ok ->
            try
                Config = get_filename(),
                db_tools_dict:set_db_name(Config),
                db_tools_operation:do_connect_db(),
                db_tools_operation:do_create_db(),
                db_tools_operation:do_use_database(),
                db_tools_operation:do_create_tables(Config),
                db_tools_operation:do_alter_tables(Config)
            catch
                throw:Reason ->
                    ?CONSOLE("执行失败:~tp", [Reason]);
                Err:Reason:Track ->
                    ?CONSOLE("Err:~w Reason:~w~nTrack:~tp", [Err, Reason, Track])
            end;
        help ->
            db_tools_helper:help()
    end,
    do_close_export(),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

get_filename() ->
    Filename = db_tools_dict:get_config_filename(),
    {ok, [Config]} = file:consult(Filename),
    Config.

do_export_db(SQL) ->
    file:write(db_tools_dict:get_export(), <<SQL/binary, "\n\n">>).

do_close_export() ->
    case db_tools_dict:get_export() of
        undefined ->
            ok;
        IO when is_pid(IO) ->
            file:close(IO)
    end.