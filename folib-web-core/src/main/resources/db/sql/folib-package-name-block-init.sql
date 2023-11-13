DROP TABLE IF EXISTS `package_name_block`;

CREATE TABLE `package_name_block` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT '主键ID',
  `package_name` varchar(255) COLLATE utf8mb4_bin DEFAULT '' COMMENT '包名',
  `condition_value` varchar(255) COLLATE utf8mb4_bin DEFAULT '' COMMENT '条件 range（范围）eq（等于）',
  `version` varchar(255) COLLATE utf8mb4_bin DEFAULT '' COMMENT '版本',
  `create_by` varchar(255) COLLATE utf8mb4_bin DEFAULT '' COMMENT '创建人',
  `create_time` datetime DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `update_by` varchar(255) COLLATE utf8mb4_bin DEFAULT '' COMMENT '更新人',
  `update_time` datetime DEFAULT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`),
  KEY `idx_package_name` (`package_name`) USING BTREE
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin COMMENT='包名阻断';