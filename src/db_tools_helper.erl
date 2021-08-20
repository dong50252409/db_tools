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

%% 解析输出参数
-spec parse_args(list()) -> ok|help.
parse_args(["--help" | _T]) ->
    help;
parse_args(["--verbose" | T]) ->
    db_tools_dict:set_verbose(),
    parse_args(T);
parse_args([Param, Filename | T]) when Param =:= "-f"; Param =:= "--filename" ->
    db_tools_dict:set_config_filename(Filename),
    parse_args(T);
parse_args([Param, ModStr | T]) when Param =:= "-m"; Param =:= "--mode" ->
    db_tools_dict:set_mode(ModStr),
    parse_args(T);
parse_args(["--character", DBCharacter | T]) ->
    db_tools_dict:set_db_character(DBCharacter),
    parse_args(T);
parse_args(["--collation", DBCollation | T]) ->
    db_tools_dict:set_db_collation(DBCollation),
    parse_args(T);
parse_args([Param, DBHost | T]) when Param =:= "-h"; Param =:= "--host" ->
    db_tools_dict:set_db_host(DBHost),
    parse_args(T);
parse_args([Param, DBPort | T]) when Param =:= "-P"; Param =:= "--port" ->
    db_tools_dict:set_db_port(list_to_integer(DBPort)),
    parse_args(T);
parse_args([Param, DBUser | T]) when Param =:= "-u"; Param =:= "--user" ->
    db_tools_dict:set_db_user(DBUser),
    parse_args(T);
parse_args([Param, DBPasswd | T]) when Param =:= "-p"; Param =:= "--passwd" ->
    db_tools_dict:set_db_passwd(DBPasswd),
    parse_args(T);
parse_args([Param, DBName | T]) when Param =:= "-n"; Param =:= "--db_name" ->
    db_tools_dict:set_db_name(DBName),
    parse_args(T);
parse_args([Param, ExportFilename | T]) when Param =:= "-e"; Param =:= "--export" ->
    db_tools_dict:set_export_filename(ExportFilename),
    parse_args(T);
parse_args(["--not_del_tbl" | T]) ->
    db_tools_dict:set_not_del_tbl(),
    parse_args(T);
parse_args(["--not_del_field" | T]) ->
    db_tools_dict:set_not_del_field(),
    parse_args(T);
parse_args(["--out_hrl", OutHrl | T]) ->
    db_tools_dict:set_out_hrl(OutHrl),
    parse_args(T);
parse_args(["--hrl_prefix", HRLPrefix | T]) ->
    db_tools_dict:set_hrl_prefix(HRLPrefix),
    parse_args(T);
parse_args(["--out_erl", OutERL | T]) ->
    db_tools_dict:set_out_erl(OutERL),
    parse_args(T);
parse_args(["--erl_prefix", ERLPrefix | T]) ->
    db_tools_dict:set_erl_prefix(ERLPrefix),
    parse_args(T);
parse_args([]) ->
    ok.

%% 打印使用帮助
-spec help() -> ok.
help() ->
    Help = [
        "数据库表结构管理工具",
        "用法 db_tools [--help] [--verbose] [OPTIONS]",
        "   --help                      显示帮助说明",
        "   --verbose                   显示详细内容",
        "可用的选项（需要指定额外参数，使用空格分隔每个参数）",
        "   -f          --filename      指定配置表文件位置",
        "   -m          --mode          指定执行模式，可选模式如下",
        "                               update_db 创建更新数据库以及表结构",
        "                               truncate_db 仅执行截断数据库表（清库）",
        "                               gen_model 生成数据库表对应Erlang的实体文件",
        "",
        "数据库管理相关可用选项",
        "   --character                 指定数据库以及表字符集，不指定则为默认设置",
        "   --collation                 指定数据库以及表排序规则，不指定则为默认设置",
        "   -h          --host          指定MySQL主机地址（默认：~ts）",
        "   -P          --port          指定MySQL端口号（默认：~w）",
        "   -u          --user          指定MySQL用户名",
        "   -p          --passwd        指定MySQL密码",
        "   -n          --db_name       指定数据库名",
        "   -e          --export        指定导出SQL语句的文件名（默认：不导出）",
        "   --not_del_tbl               指定更新时不删除配置表文件中不存在的数据库表，无额外参数（默认：删除配置表中不存在的数据库表）",
        "   --not_del_field             指定更新时不删除配置表文件中不存在的表字段，无额外参数（默认：删除配置表中不存在的字段）",
        "",
        "Erlang文件生成相关可用选项",
        "   --out_hrl                   指定HRL文件输出位置（默认：当前目录）",
        "   --hrl_prifix                指定统一增加HRL文件前缀",
        "   --out_erl                   指定ERL文件输出位置（默认：当前目录）",
        "   --erl_prifix                指定统一增加ERL文件浅醉"
    ],
    Str = io_lib:format(lists:flatten(lists:join("\n", Help)), [?DEFAULT_HOST, ?DEFAULT_PORT]),
    ?CONSOLE("~ts", [Str]).