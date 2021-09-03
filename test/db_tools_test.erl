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

update_db_test() ->
    Args = [
        "--verbose",
        "-m", "update_db",
        "--character", "utf8mb4",
        "--collation", "utf8mb4_general_ci",
        "-f", "test.config",
        "-u", "root",
        "-p", "root",
        "-n", "test_db",
        "-e", "test_db.sql"
    ],
    db_tools:main(Args).


truncate_db_test() ->
    DBUser = "root",
    DBPasswd = "root",
    Args = [
        "--verbose",
        "-m", "truncate_db",
        "-u", DBUser,
        "-p", DBPasswd
    ],
    db_tools:main(Args).

gen_entity_test() ->
    Args = [
        "--verbose",
        "-f","test_db.config",
        "-m", "gen_model",
        "--out_hrl", "./hrl/",
        "--hrl_prefix", "tbl_",
        "--out_erl", "./erl/",
        "--erl_prefix", "tbl_"
    ],
    db_tools:main(Args).