%%%-------------------------------------------------------------------
%%% @author gz1417
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 展示使用帮助、解析参数列表
%%% @end
%%% Created : 03. 8月 2021 11:59
%%%-------------------------------------------------------------------
-module(db_tools_helper).

-include("db_tools.hrl").

%% API
-export([parse_args/1, help/0]).

parse_args(["--help" | _T]) ->
    help;
parse_args(["--verbose" | T]) ->
    db_tools_dict:set_verbose(),
    parse_args(T);
parse_args(["--character", DBCharacter | T]) ->
    db_tools_dict:set_db_character(DBCharacter),
    parse_args(T);
parse_args(["--collation", DBCollation | T]) ->
    db_tools_dict:set_db_collation(DBCollation),
    parse_args(T);
parse_args(["-f", Filename | T]) ->
    db_tools_dict:set_config_filename(Filename),
    parse_args(T);
parse_args(["-h", DBHost | T]) ->
    db_tools_dict:set_db_host(DBHost),
    parse_args(T);
parse_args(["-P", DBPort | T]) ->
    db_tools_dict:set_db_port(list_to_integer(DBPort)),
    parse_args(T);
parse_args(["-u", DBUser | T]) ->
    db_tools_dict:set_db_user(DBUser),
    parse_args(T);
parse_args(["-p", DBPasswd | T]) ->
    db_tools_dict:set_db_passwd(DBPasswd),
    parse_args(T);
parse_args(["-n", DBName | T]) ->
    db_tools_dict:set_db_name(DBName),
    parse_args(T);
parse_args(["-e", ExportFilename | T]) ->
    db_tools_dict:set_export_filename(ExportFilename),
    parse_args(T);
parse_args([]) ->
    ok.

help() ->
    Help = [
        "数据库表结构管理工具",
        "--help         显示帮助说明",
        "--verbose      输出详细内容",
        "--character    指定数据库以及表字符集，不指定则为默认设置",
        "--collation    指定数据库以及表排序规则，不指定则为默认设置",
        "可用选项（需要指定额外参数，使用空格分隔每个参数）",
        "-f             指定配置表文件位置",
        "-h             指定MySQL主机地址（默认：~ts）",
        "-P             指定MySQL端口号（默认：~w）",
        "-u             指定MySQL用户名",
        "-p             指定MySQL密码",
        "-n             指定数据库名（默认：通过指定的配置表获取）",
        "-e             指定导出SQL语句的文件名（默认：不导出）"
    ],
    ?CONSOLE(lists:flatten(lists:join("\n", Help)), [?DEFAULT_HOST, ?DEFAULT_PORT]).