DROP TABLE IF EXISTS `dict`;

CREATE TABLE `dict` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT '主键ID',
  `create_time` datetime DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `dict_type` varchar(20) COLLATE utf8mb4_bin DEFAULT 'upload_process' COMMENT '字典类型',
  `dict_key` varchar(50) COLLATE utf8mb4_bin NOT NULL COMMENT '字典key',
  `dict_value` varchar(255) COLLATE utf8mb4_bin DEFAULT '' COMMENT '字典value',
  `alias` varchar(255) COLLATE utf8mb4_bin DEFAULT '' COMMENT '别名',
  `comment` varchar(255) COLLATE utf8mb4_bin DEFAULT '' COMMENT '备注',
  PRIMARY KEY (`id`),
  KEY `idx_dict_key` (`dict_key`) USING BTREE,
  KEY `idx_dict_type` (`dict_type`) USING BTREE
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin COMMENT='字典';
