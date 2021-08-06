CREATE DATABASE IF NOT EXISTS `test_db`;

CREATE TABLE IF NOT EXISTS `tbl_1` (
	`field_1` bigint(20) unsigned zerofill NOT NULL AUTO_INCREMENT COMMENT '字段1',
	`field_2` varchar(50) COLLATE utf8_general_ci NOT NULL DEFAULT '' COMMENT '字段2',
	`field_3` tinyint(11) unsigned NOT NULL DEFAULT '0' COMMENT '字段3',
	`field_4` int NOT NULL COMMENT '字段4',
	`field_5` json NOT NULL COMMENT '字段5',
	`field_6` text NOT NULL COMMENT '字段7',
	PRIMARY KEY `primary_key` (`field_1`) USING BTREE,
	UNIQUE KEY `unique_key` (`field_2`) USING HASH COMMENT '唯一id',
	KEY `normal_key` (`field_3`,`field_4`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci COMMENT='测试表1';

CREATE TABLE IF NOT EXISTS `tbl_2` (
	`field_1` bigint(20) unsigned zerofill AUTO_INCREMENT,
	`field_2` varchar(50) COLLATE utf8_general_ci,
	`field_3` tinyint(11) unsigned,
	`field_4` int,
	`field_5` json,
	`field_6` text,
	PRIMARY KEY (`field_1`),
	UNIQUE KEY (`field_2`),
	KEY (`field_3`,`field_4`)
) COMMENT='测试表2';

DESC `tbl_1`;

