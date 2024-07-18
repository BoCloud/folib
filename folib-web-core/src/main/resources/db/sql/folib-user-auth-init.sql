DROP TABLE IF EXISTS `folib_user`;
-- folib_scanner.folib_user definition

CREATE TABLE `folib_user` (
  `id` bigint NOT NULL AUTO_INCREMENT COMMENT '主键',
  `username` varchar(255) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '用户名',
  `password` varchar(255) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '密码',
  `original_password` varchar(255) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '原始密码',
  `avatar` varchar(255) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '头像',
  `email` varchar(64) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '邮件',
  `user_type` varchar(20) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '用户类型',
  `enabled` varchar(1) COLLATE utf8mb4_unicode_ci DEFAULT '0' COMMENT '是否启用',
  `source_id` varchar(255) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '来源',
  `deleted` varchar(1) COLLATE utf8mb4_unicode_ci DEFAULT '0' COMMENT '是否删除',
  `create_by` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT '' COMMENT '创建人',
  `create_time` datetime DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `update_by` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT '' COMMENT '更新人',
  `update_time` datetime DEFAULT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='用户信息';

INSERT INTO folib_scanner.user_group (id, group_name, description, join_group, is_default, deleted, create_by, create_time, update_by, update_time) VALUES(1, 'default_group', '默认用户组', '1', '1', '0', NULL, now(), NULL, now());
