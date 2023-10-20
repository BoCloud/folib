DROP TABLE IF EXISTS `artifact_sync_record`;

CREATE TABLE `artifact_sync_record` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT '主键ID',
  `source_path` text COMMENT '源制品路径',
  `target_path` text COMMENT '目标制品路径',
  `sync_no` varchar(64) DEFAULT NULL COMMENT '制品同步编号',
  `ops_type` int(11) DEFAULT NULL COMMENT '制品操作（1：制品晋级；2：制品分发）',
  `sync_model` int(11) DEFAULT NULL COMMENT '同步模式（1：推；2：拉）',
  `status` int(11) DEFAULT NULL COMMENT '同步状态（1：就绪；2：同步中；3：成功；4：失败）',
  `failed_reason` text COMMENT '失败的原因',
  `retry` int(11) DEFAULT NULL COMMENT '是否重试（0:不重试，1:重试）',
  `created_by` varchar(255) DEFAULT NULL COMMENT '创建人',
  `created_time` datetime DEFAULT NULL COMMENT '创建时间',
  `updated_by` varchar(255) DEFAULT NULL COMMENT '更新人',
  `updated_time` datetime DEFAULT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`),
  UNIQUE KEY `idx_sync_no` (`sync_no`) USING BTREE,
  KEY `idx_ops_type` (`ops_type`) USING BTREE,
  KEY `idx_status` (`status`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='制品同步记录';