db_tools
=====

MySQL数据库表结构自动更新管理工具

编译
-----

    $ rebar3 escriptize

使用帮助
---

    $ _build/default/bin/db_tools --help

主要模式功能
----

* `update_db` 读取指定的配置文件，创建更新数据库表结构
* `truncate_db` 读取指定的配置文件，并清空相关数据库表数据
* `gen_model` 读取指定的配置文件，生成数据库表对应Erlang的实体文件
* `gen_model_by_db` 读取指定的数据库表结构SQL文件，生成数据库表对应Erlang的实体文件
* 配合 [`db`](https://github.com/dong50252409/db) 应用可实现自动的表数据持久化功能

文件结构定义
---

配置表结构为

```[[表选项, 表字段列, 索引列, 扩展字段列], ...]```

**表选项**：包含数据库表名、表描述（可选）

```
{table, [name(), comment()]}

-type name() :: {name, atom()}.

-type comment() :: {comment, string()}.
```

**表字段**：包含字段名、字段类型、非空（可选）、主键自增（可选）、默认值（可选）、描述（可选）、to_term指定则自动将数据转为Erlang项式（仅针对`json、text、blob`类型）

```
{fields, [[name(), field_type(), not_null(), auto_inc(), field_default(), comment(), to_term()], ...]}

-type name() :: {name, atom()}.

-type field_type() :: {type, string()}.

-type not_null() :: not_null.

-type auto_inc() :: auto_inc. 

-type field_default() :: {default, number() | string()}.

-type comment() :: {comment, string()}.

-type to_term() :: to_term.
```

**索引列**：包含索引字段名、索引类型（可选，默认值`normal`）

```
{index, [[index_fields(), index_type()], ...]}

-type index_fields() :: [atom(), ...].

-type index_type() :: primary | unique | normal.
```

**扩展列**：包含字段名、字段默认值（可选，默认值`undefined`）、描述（可选）

```
{extend_fields, [[name(), extend_default(), comment()], ...]}

-type name() :: {name, atom()}.

-type extend_default() :: {default, any()}.

-type comment() :: {comment, string()}.
```

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

* 无法重命名表，因为无法从配置文件中获取原有表名，目前的做法是新建表，并删除配置表中不存在的表，可以通过在指定 `--keep_tables` 保留配置表中不存在的表

* 无法重命名字段，因为无法配置文件中获取原有字段名，目前的做法是创建新字段，并删除配置表中不存在的字段，可以通过指定 `--keep_fields` 保留配置表中不存在的字段