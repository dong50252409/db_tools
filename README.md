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

    {数据库名称, [[表选项, 表字段列, 索引列, 扩展字段列], ...]}.
    数据库名称：默认数据库名，可通过参数-n指定数据库名称
    表选项：{table, [name(), engine(), charset(), collate(), comment()]}
    表字段：{fields, [[name(), field_type(), not_null(), auto_inc(), field_default(), comment()], ...]}
    索引列：{index, [[name(), index_fields(), index_type(), index_func(), comment()], ...]}
    扩展列：{extend_fields, [[name(), extend_type(), extend_default(), comment()], ...]}
    
    -type name() :: {name, atom()}.
    指定表、字段、索引以及扩展字段的名称，其中索引名称可以省略，默认取第一个字段名作为其索引名称
    
    -type field_type() :: {type, string()}.
    指定字段类型，理论上支持MySQL所有可用类型，直接写入相应字符即可，例如 "int(11)"、"int unsigned"、"bigint unsigned zerofill"、"varchar(50) COLLATE utf8mb4_bin"、"json"、"text"等
    
    -type not_null() :: not_null.
    指定则字段不能为空，省略则可以为null
    
    -type auto_inc() :: auto_inc.
    指定则字段自动递增
    
    -type field_default() :: {default, number() | string()}.
    指定字段默认值，省略则无默认值
    
    -type index_fields() :: [atom(), ...].
    指定索引字段
    
    -type index_type() :: primary | unique | normal.
    指定索引类型，省略则取值为normal
    
    -type index_func() :: btree | hash.
    指定索引方法，省略则取默认配置值
    
    -type engine() :: {engine, string()}.
    指定表使用的数据库引，例如 InnoDB、MEMORY、MYISAM等，省略则取默认值
    
    -type charset() :: {charset, string()}.
    指定表使用的字符集，省略则取默认配置值
    
    -type collate() :: {collate, string()}.
    指定表的排序规则，省略则取默认配置值
    
    -type comment() :: {comment, string()}.
    指定表、字段、索引以及扩展字段的注释，省略则为空
    
    -type extend_type() :: {type, atom()}.
    指定扩展字段类型，可使用所有Erlang中允许的数据类型，省略则为undefined
    
    -type extend_default() :: {default, any()}.
    指定扩展字段默认值，可使用所有Erlang中允许的数据类型，省略则为undefined