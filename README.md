db_tools
=====

MySQL数据库表结构自动更新管理工具

编译
-----

    $ rebar3 escriptize

使用帮助
---

    $ _build/default/bin/db_tools --help

配置文件定义
---

    [[表选项, 表字段列, 索引列, 扩展字段列], ...].
    表选项：{table, [name(), comment()]}
    表字段：{fields, [[name(), field_type(), not_null(), auto_inc(), field_default(), comment()], ...]}
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
    
    -type extend_default() :: {default, any()}.
    指定扩展字段默认值，可使用所有Erlang中允许的数据类型，省略则为undefined

简单的模板
----
    [
        [
            {table, [{name, test_1}, {comment, "测试表1"}]},
            {fields, [
                [{name, field_1}, {type, "bigint(20) unsigned zerofill"}, not_null, auto_inc, {comment, "字段1"}],
                [{name, field_2}, {type, "varchar(50)"}, not_null, {default, ""}, {comment, "字段2"}],
                [{name, field_3}, {type, "tinyint(3) unsigned"}, not_null, {default, 0}, {comment, "字段3"}],
                [{name, field_4}, {type, "int(11)"}, not_null, {comment, "字段4"}],
                [{name, field_5}, {type, "json"}, not_null, {comment, "字段5"}],
                [{name, field_6}, {type, "text"}, not_null, {comment, "字段6"}]
            ]},
            {index, [
                [{fields, [field_1]}, primary],
                [{fields, [field_3, field_4]}, normal],
                [{fields, [field_2]}, unique]
            ]},
            {extend_fields, [
                [{name, ext_field_1}, {default, 0}, {comment, "扩展字段1"}],
                [{name, ext_field_2}, {default, []}, {comment, "扩展字段2"}],
                [{name, ext_field_3}, {default, {}}, {comment, "扩展字段3"}],
                [{name, ext_field_4}, {default, <<>>}, {comment, "扩展字段4"}]
            ]}
        ],
            
        %% 这个是简略版
        [
            {table, [{name, test_2}, {comment, "测试表2"}]},
            {fields, [
                [{name, field_1}, {type, "bigint(20) unsigned zerofill"}, not_null],
                [{name, field_2}, {type, "varchar(50)"}],
                [{name, field_3}, {type, "tinyint(3) unsigned"}],
                [{name, field_4}, {type, "int(11)"}],
                [{name, field_5}, {type, "json"}],
                [{name, field_6}, {type, "text"}]
            ]},
            {index, [
                [{fields, [field_1]}, primary],
                [{fields, [field_3, field_4]}],
                [{fields, [field_2]}, unique]
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