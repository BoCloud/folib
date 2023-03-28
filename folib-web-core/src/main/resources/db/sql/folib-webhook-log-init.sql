DROP TABLE IF EXISTS `webhook_log`;

CREATE TABLE `webhook_log` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT '主键ID',
  `create_time` datetime DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `event_type` varchar(255) COLLATE utf8mb4_bin DEFAULT '' COMMENT '事件类型',
  `storage_id` varchar(255) COLLATE utf8mb4_bin DEFAULT '' COMMENT '存储空间名称',
  `repository_id` varchar(255) COLLATE utf8mb4_bin DEFAULT '' COMMENT '仓库名称',
  `artifact_path` varchar(255) COLLATE utf8mb4_bin DEFAULT '' COMMENT '制品路径',
  `url` varchar(255) COLLATE utf8mb4_bin DEFAULT '' COMMENT '请求url',
  `access_token` varchar(255) COLLATE utf8mb4_bin DEFAULT '' COMMENT '访问令牌',
  `method` varchar(255) COLLATE utf8mb4_bin DEFAULT '' COMMENT '请求方式',
  `completion_time` decimal(16,2) DEFAULT '0.00' COMMENT '完成时间（秒）',
  `request_headers` text COLLATE utf8mb4_bin COMMENT '请求头',
  `request` longtext COLLATE utf8mb4_bin COMMENT '请求报文',
  `response_status` varchar(255) COLLATE utf8mb4_bin DEFAULT '200' COMMENT '响应状态码',
  `response_headers` text COLLATE utf8mb4_bin COMMENT '响应头',
  `response` longtext COLLATE utf8mb4_bin COMMENT '响应报文',
  `remark` varchar(255) COLLATE utf8mb4_bin DEFAULT '' COMMENT '备注',
  PRIMARY KEY (`id`),
  KEY `idx_storage_id` (`storage_id`) USING BTREE,
  KEY `idx_repository_id` (`repository_id`) USING BTREE,
  KEY `idx_artifact_path` (`artifact_path`) USING BTREE
) ENGINE=InnoDB AUTO_INCREMENT=417 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin COMMENT='webhook';
