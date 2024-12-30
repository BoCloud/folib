DROP TABLE IF EXISTS `custom_layout`;

CREATE TABLE `custom_layout` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT '主键ID',
  `layout_name` varchar(255) COLLATE utf8mb4_bin DEFAULT '' COMMENT '布局名称',
  `artifact_path_pattern` varchar(1000) COLLATE utf8mb4_bin DEFAULT '' COMMENT '制品路径正则表达式',
  `create_by` varchar(255) COLLATE utf8mb4_bin DEFAULT '' COMMENT '创建人',
  `create_time` datetime DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `update_by` varchar(255) COLLATE utf8mb4_bin DEFAULT '' COMMENT '更新人',
  `update_time` datetime DEFAULT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`),
  UNIQUE KEY `uk_layout_name` (`layout_name`) USING BTREE,
  KEY `idx_create_time` (`create_time`) USING BTREE
) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin COMMENT='自定义布局';

INSERT INTO custom_layout
(id, layout_name, artifact_path_pattern, create_by, create_time, update_by, update_time)
VALUES(924597878351986689, 'maven-2-layout', '(?<orgPath>.+?)/(?<module>[^/]+)/(?<baseRev>[^/]+?)(?:-(?<folderItegRev>SNAPSHOT))?/\\k<module>-\\k<baseRev>(?:-(?<fileItegRev>SNAPSHOT|(?:\\d{8}\\.\\d{6}-\\d+)))?(?:-(?<classifier>[^/]+?))?\\.(?<ext>[^\\-/]+|7z)', 'admin', '2024-12-30 09:00:00', 'admin', '2024-12-30 09:00:00'),
(924952958964072449, 'simple-layout', '(?<orgPath>.+?)/(?<module>[^/]+)/\\k<module>-(?<baseRev>[^/]+?)\\.(?<ext>(?:(?!\\d))[^\\-/]+|7z)', 'admin', '2024-12-30 09:00:00', 'admin', '2024-12-30 09:00:00');