DROP TABLE IF EXISTS `backup_strategy`;

CREATE TABLE `backup_strategy` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT '主键ID',
  `enabled` int(3) DEFAULT '1' COMMENT '是否启用 1 启用 0 不启用',
  `strategy_name` varchar(255) COLLATE utf8mb4_bin DEFAULT '' COMMENT '备份策略名称',
  `cron_expression` varchar(255) COLLATE utf8mb4_bin DEFAULT '' COMMENT 'cron定时设置',
  `backup_path` varchar(255) COLLATE utf8mb4_bin DEFAULT '' COMMENT '备份路径',
  `incremental` int(3) DEFAULT '0' COMMENT '增量备份 1 是 0 否',
  `retention_period` int(11) DEFAULT '0' COMMENT '全量备份保留期限',
  `create_by` varchar(255) COLLATE utf8mb4_bin DEFAULT '' COMMENT '创建人',
  `create_time` datetime DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `update_by` varchar(255) COLLATE utf8mb4_bin DEFAULT '' COMMENT '更新人',
  `update_time` datetime DEFAULT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`),
  KEY `idx_enabled` (`enabled`) USING BTREE,
  KEY `idx_strategy_name` (`strategy_name`) USING BTREE,
  KEY `idx_incremental` (`incremental`) USING BTREE,
  KEY `idx_create_time` (`create_time`) USING BTREE
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin COMMENT='备份策略';

DROP TABLE IF EXISTS `backup_strategy_repository`;

CREATE TABLE `backup_strategy_repository` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT '主键ID',
  `backup_strategy_id` bigint(20) DEFAULT '0' COMMENT '备份策略ID',
  `storage_id` varchar(255) COLLATE utf8mb4_bin DEFAULT '' COMMENT '存储空间名称',
  `repository_id` varchar(255) COLLATE utf8mb4_bin DEFAULT '' COMMENT '仓库名称',
  `create_by` varchar(255) COLLATE utf8mb4_bin DEFAULT '' COMMENT '创建人',
  `create_time` datetime DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `update_by` varchar(255) COLLATE utf8mb4_bin DEFAULT '' COMMENT '更新人',
  `update_time` datetime DEFAULT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`),
  KEY `idx_backup_strategy_id` (`backup_strategy_id`) USING BTREE,
  KEY `idx_storage_id` (`storage_id`) USING BTREE,
  KEY `idx_repository_id` (`repository_id`) USING BTREE,
  KEY `idx_create_time` (`create_time`) USING BTREE
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin COMMENT='备份策略仓库';