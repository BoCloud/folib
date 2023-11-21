DROP TABLE IF EXISTS `artifact_sync_record`;

CREATE TABLE `artifact_sync_record` (
  `id` BIGINT(20) NOT NULL AUTO_INCREMENT COMMENT '主键ID',
  `source_path` text COMMENT '源制品路径',
  `target_path` text COMMENT '目标制品路径',
  `sync_no` VARCHAR(64) COLLATE utf8mb4_bin DEFAULT '' COMMENT '制品同步编号',
  `ops_type` INT(11) DEFAULT 1 COMMENT '制品操作（1：制品晋级；2：制品分发）',
  `sync_model` INT(11) DEFAULT 2 COMMENT '同步模式（1：推；2：拉）',
  `status` INT(11) DEFAULT 1 COMMENT '同步状态（1：就绪；2：同步中；3：成功；4：失败）',
  `failed_reason` text COMMENT '失败的原因',
  `retry` INT(11) DEFAULT 0 COMMENT '是否重试（0:不重试，1:重试）',
  `create_by` VARCHAR ( 255 ) COLLATE utf8mb4_bin DEFAULT '' COMMENT '创建人',
  `create_time` datetime DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `update_by` VARCHAR ( 255 ) COLLATE utf8mb4_bin DEFAULT '' COMMENT '更新人',
  `update_time` datetime DEFAULT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`),
  UNIQUE KEY `uk_sync_no` (`sync_no`) USING BTREE,
  KEY `idx_ops_type` (`ops_type`) USING BTREE,
  KEY `idx_status` (`status`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='制品同步记录';