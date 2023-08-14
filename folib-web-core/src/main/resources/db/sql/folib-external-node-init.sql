DROP TABLE IF EXISTS `external_node`;

CREATE TABLE `external_node` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT '主键ID',
  `create_time` datetime DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `node_name` varchar(255) COLLATE utf8mb4_bin DEFAULT '' COMMENT '节点名称',
  `type` varchar(255) COLLATE utf8mb4_bin DEFAULT '' COMMENT '制品库类型',
  `address` varchar(512) COLLATE utf8mb4_bin DEFAULT '' COMMENT '节点地址',
  `username` varchar(255) COLLATE utf8mb4_bin DEFAULT '' COMMENT '用户名',
  `password` varchar(512) COLLATE utf8mb4_bin DEFAULT '' COMMENT '密码',
  `comment` varchar(255) COLLATE utf8mb4_bin DEFAULT '' COMMENT '备注',
  PRIMARY KEY (`id`),
  UNIQUE KEY `uk_node_name` (`node_name`) USING BTREE,
  KEY `idx_type` (`type`) USING BTREE
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin COMMENT='外部节点';
