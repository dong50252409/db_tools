%%%-------------------------------------------------------------------
%%% @author gz1417
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 8月 2021 13:58
%%%-------------------------------------------------------------------
-module(db_tools_test).

-include_lib("eunit/include/eunit.hrl").

parse_args_test() ->
    ConfigFilename = "test.config",
    DBHost = "127.0.0.1",
    DBPort = 3306,
    DBUser = "root",
    DBPasswd = "root",
    DBName = "test_db",
    ExportFilename = "test_db.sql",
    Args = [
        "-f", ConfigFilename,
        "-h", DBHost,
        "-P", integer_to_list(DBPort),
        "-u", DBUser,
        "-p", DBPasswd,
        "-n", DBName,
        "-e", ExportFilename,
        "--verbose",
        "--help"
    ],
    ?assertMatch(help, db_tools_helper:parse_args(Args)),
    ?assertMatch(ConfigFilename, db_tools_dict:get_config_filename()),
    ?assertMatch(DBHost, db_tools_dict:get_db_host()),
    ?assertMatch(DBPort, db_tools_dict:get_db_port()),
    ?assertMatch(DBUser, db_tools_dict:get_db_user()),
    ?assertMatch(DBPasswd, db_tools_dict:get_db_passwd()),
    ?assertMatch(DBName, db_tools_dict:get_db_name()),
    ?assertMatch(ExportFilename, db_tools_dict:get_export_filename()),
    ?assert(db_tools_dict:is_verbose()).

help_test() ->
    db_tools_helper:help().
