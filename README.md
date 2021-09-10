db_tools
=====

MySQL数据库表结构自动更新管理工具

主要功能
----
* update_db 读取 **`*.config`** 文件，创建更新数据库表结构
* truncate_db 读取 **`*.config`** 文件，并清空相关数据库表数据
* gen_model 读取 **`*.config`** 文件，生成数据库表对应Erlang的实体文件
* 配合 [`db`](https://github.com/dong50252409/db) 应用可实现半自动数据库持久化功能

编译
-----

    $ rebar3 escriptize

使用帮助
---

    $ _build/default/bin/db_tools --help

配置文件定义
---

    [[表选项, 表字段列, 索引列, 扩展字段列], ...]
    其中表选项、表字段、索引列用于生成创建、修改数据库表的SQL语句
    扩展列用于生成model文件中的额外字段

    表选项：{table, [name(), comment()]}
    表字段：{fields, [[name(), field_type(), not_null(), auto_inc(), field_default(), comment(), to_term()], ...]}
    索引列：{index, [[index_fields(), index_type()], ...]}
    扩展列：{extend_fields, [[name(), extend_default(), comment()], ...]}
    
    -type name() :: {name, atom()}.
    指定表、字段、扩展字段的名称

    -type field_type() :: {type, string()}.
    指定字段类型，理论上支持MySQL所有可用类型，直接写入相应字符即可
    例如 "int(11)"、"bigint(20) unsigned zerofill"、"varchar(50) COLLATE utf8mb4_bin"、"json"、"text"等
    
    -type not_null() :: not_null.
    指定则字段不能为空，省略则可以为null
    
    -type auto_inc() :: auto_inc.
    指定则字段自动递增，auto_inc()和field_default()只能使用其中一个选项，且字段必须为主键
    
    -type field_default() :: {default, number() | string()}.
    指定字段默认值，省略则无默认值
    
    -type index_fields() :: [atom(), ...].
    指定索引字段
    
    -type index_type() :: primary | unique | normal.
    指定索引类型，省略则取值为normal
    
    -type comment() :: {comment, string()}.
    指定表、字段以及扩展字段的注释，省略则为空
    
    -type to_term() :: to_term.
    指定将json、text、blob类型的字段转为Erlang term
    
    -type extend_default() :: {default, any()}.
    指定扩展字段默认值，可使用所有Erlang中允许的数据类型，省略则为undefined

简单的模板
----
    [
        [
            {table, [{name, test_1}, {comment, "测试表1"}]},
            {fields, [
                [{name, field_1}, {type, "bigint(20) unsigned zerofill"}, not_null, auto_inc, {comment, "字段1"}],
                [{name, field_2}, {type, "tinyint(3) unsigned"}, not_null, {comment, "字段2"}],
                [{name, field_3}, {type, "varchar(50)"}, not_null, {default, ""}, {comment, "字段3"}],
                [{name, field_4}, {type, "int(11)"}, not_null, {default, 0}, {comment, "字段4"}],
                [{name, field_5}, {type, "json"}, not_null, {comment, "字段5"}, to_term],
                [{name, field_6}, {type, "text"}, not_null, {comment, "字段6"}, to_term],
                [{name, field_7}, {type, "blob"}, not_null, {comment, "字段6"}, to_term]
            ]},
            {index, [
                [{fields, [field_1, field_2]}, primary],
                [{fields, [field_3]}, unique],
                [{fields, [field_4]}, normal]
            ]},
            {extend_fields, [
                [{name, ext_field_1}, {default, 0}, {comment, "扩展字段1"}],
                [{name, ext_field_2}, {default, []}, {comment, "扩展字段2"}],
                [{name, ext_field_3}, {default, {}}, {comment, "扩展字段3"}],
                [{name, ext_field_4}, {default, <<>>}, {comment, "扩展字段4"}]
            ]}
        ],
        [
            {table, [{name, test_2}, {comment, "测试表2"}]},
            {fields, [
                [{name, field_1}, {type, "bigint(20) unsigned zerofill"}, not_null, auto_inc],
                [{name, field_2}, {type, "tinyint(3) unsigned"}, not_null],
                [{name, field_3}, {type, "varchar(50)"}],
                [{name, field_4}, {type, "int(11)"}],
                [{name, field_5}, {type, "json"}, to_term],
                [{name, field_6}, {type, "text"}, to_term],
                [{name, field_7}, {type, "blob"}, to_term]
            ]},
            {index, [
                [{fields, [field_1, field_2]}, primary],
                [{fields, [field_3]}, unique],
                [{fields, [field_4]}]
            ]},
            {extend_fields, [
                [{name, ext_field_1}],
                [{name, ext_field_2}],
                [{name, ext_field_3}],
                [{name, ext_field_4}]
            ]}
        ]
    ].

一些限制
-----
* 无法重命名表，因为无法从配置文件中获取原有表名，目前的做法是新建表，并删除配置表中不存在的表，可以通过在指定 **--not_del_tbl** 保留配置表中不存在的表

* 无法重命名字段，因为无法配置文件中获取原有字段名，目前的做法是创建新字段，并删除配置表中不存在的字段，可以通过指定 **--not_del_field** 保留配置表中不存在的字段