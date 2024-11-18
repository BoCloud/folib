DROP TABLE IF EXISTS `block_strategy`;
CREATE TABLE `block_strategy` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT '主键ID',
  `block_strategy_name` varchar(255) COLLATE utf8mb4_bin DEFAULT '' COMMENT '阻断策略名称',
  `vulnerability_levels` varchar(255) COLLATE utf8mb4_bin DEFAULT '' COMMENT '漏洞阻断级别',
  `filter_vulnerability_whites` int(3) DEFAULT '0' COMMENT '过滤漏洞白名单',
  `filter_vulnerability_blacks` int(3) DEFAULT '0' COMMENT '过滤漏洞黑名单',
  `filter_license_whites` int(3) DEFAULT '0' COMMENT '过滤license白名单',
  `filter_license_blacks` int(3) DEFAULT '0' COMMENT '过滤license黑名单',
  `filter_all_package_name` int(3) DEFAULT '0' COMMENT '全量包名',
  `filter_all_license` int(3) DEFAULT '0' COMMENT '全量license',
  `create_by` varchar(255) COLLATE utf8mb4_bin DEFAULT '' COMMENT '创建人',
  `create_time` datetime DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `update_by` varchar(255) COLLATE utf8mb4_bin DEFAULT '' COMMENT '更新人',
  `update_time` datetime DEFAULT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`),
  UNIQUE KEY `uk_block_strategy_name` (`block_strategy_name`) USING BTREE,
  KEY `idx_create_time` (`create_time`) USING BTREE
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin COMMENT='阻断策略';

DROP TABLE IF EXISTS `block_strategy_repository`;
CREATE TABLE `block_strategy_repository` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT '主键ID',
  `block_strategy_id` bigint(20) DEFAULT '0' COMMENT '阻断策略ID',
  `storage_id` varchar(255) DEFAULT '' COMMENT '存储空间名称',
  `repository_id` varchar(255) DEFAULT '' COMMENT '仓库名称',
  `create_by` varchar(255) COLLATE utf8mb4_bin DEFAULT '' COMMENT '创建人',
  `create_time` datetime DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `update_by` varchar(255) COLLATE utf8mb4_bin DEFAULT '' COMMENT '更新人',
  `update_time` datetime DEFAULT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`),
  KEY `idx_block_strategy_id` (`block_strategy_id`) USING BTREE,
  KEY `idx_storage_id` (`storage_id`) USING BTREE,
  KEY `idx_repository_id` (`repository_id`) USING BTREE,
  KEY `idx_create_time` (`create_time`) USING BTREE
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin COMMENT='阻断策略仓库信息';

DROP TABLE IF EXISTS `block_strategy_info`;
CREATE TABLE `block_strategy_info` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT '主键ID',
  `block_strategy_id` bigint(20) DEFAULT '0' COMMENT '阻断策略ID',
  `package_name` varchar(255) DEFAULT '' COMMENT '包名',
  `license_id` varchar(255) DEFAULT '' COMMENT 'license',
  `create_by` varchar(255) COLLATE utf8mb4_bin DEFAULT '' COMMENT '创建人',
  `create_time` datetime DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `update_by` varchar(255) COLLATE utf8mb4_bin DEFAULT '' COMMENT '更新人',
  `update_time` datetime DEFAULT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`),
  KEY `idx_block_strategy_id` (`block_strategy_id`) USING BTREE,
  KEY `idx_package_name` (`package_name`) USING BTREE,
  KEY `idx_license_id` (`license_id`) USING BTREE,
  KEY `idx_create_time` (`create_time`) USING BTREE
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin COMMENT='阻断策略包名license信息';

ALTER TABLE license ADD black_white_type INT(11) DEFAULT 0 NULL COMMENT '黑白名单类型 1 白名单 2 黑名单';