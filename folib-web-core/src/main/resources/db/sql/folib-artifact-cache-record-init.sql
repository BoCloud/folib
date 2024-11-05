DROP TABLE IF EXISTS `artifact_cache_record`;

CREATE TABLE `artifact_cache_record` (
  `id` bigint(20) NOT NULL COMMENT '主键ID',
  `storage_id` varchar(255) DEFAULT '' COMMENT '存储空间名称',
  `repository_id` varchar(255) DEFAULT '' COMMENT '仓库名称',
  `artifact_path` text COMMENT '制品路径',
  `artifact_path_prefix` varchar(768) DEFAULT '' COMMENT '制品路径前缀',
  `size` bigint(20) DEFAULT '0' COMMENT '制品大小',
  `md5` varchar(255) DEFAULT '' COMMENT 'md5',
  `sha1` varchar(255) DEFAULT '' COMMENT 'sha1',
  `sha256` varchar(255) DEFAULT '' COMMENT 'sha256',
  `download_count` bigint(20) DEFAULT '0' COMMENT '下载次数',
  `latest_download_time` datetime DEFAULT NULL COMMENT '最后下载时间',
  `cache_directory_path` varchar(1000) DEFAULT '' COMMENT '缓存目录',
  `cache_path` text COMMENT '缓存路径',
  `create_time` datetime DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `update_time` datetime DEFAULT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`),
  KEY `idx_storage_id` (`storage_id`) USING BTREE,
  KEY `idx_repository_id` (`repository_id`) USING BTREE,
  KEY `idx_artifact_path_prefix` (`artifact_path_prefix`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='制品缓存记录';