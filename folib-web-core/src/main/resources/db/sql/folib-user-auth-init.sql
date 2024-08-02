DROP TABLE IF EXISTS `folib_role`;
-- folib_scanner.folib_role definition

CREATE TABLE `folib_role` (
                              `id` varchar(100) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '主键',
                              `cn_name` varchar(255) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '中文名称',
                              `en_name` varchar(255) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '英文名称',
                              `description` varchar(255) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '角色描述',
                              `deleted` varchar(1) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT '0' COMMENT '是否删除',
                              `is_default` varchar(1) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT '0' COMMENT '是否默认',
                              `create_by` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT '' COMMENT '创建人',
                              `create_time` timestamp NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
                              `update_by` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT '' COMMENT '更新人',
                              `update_time` timestamp NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
                              PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='角色信息';

DROP TABLE IF EXISTS `folib_user`;
-- folib_scanner.folib_user definition

CREATE TABLE `folib_user` (
                              `id` varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '主键',
                              `username` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '用户名',
                              `password` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '密码',
                              `original_password` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '原始密码',
                              `avatar` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '头像',
                              `email` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '邮件',
                              `user_type` varchar(20) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '用户类型',
                              `enabled` varchar(20) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT 'true' COMMENT '是否启用',
                              `source_id` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '来源',
                              `deleted` tinyint(1) DEFAULT '0' COMMENT '是否删除',
                              `create_by` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT '' COMMENT '创建人',
                              `create_time` timestamp NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
                              `update_by` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT '' COMMENT '更新人',
                              `update_time` timestamp NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
                              PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='用户信息';


DROP TABLE IF EXISTS `resource`;
-- folib_scanner.resource definition

CREATE TABLE `resource` (
                            `id` bigint NOT NULL AUTO_INCREMENT COMMENT '主键',
                            `api_authoritie` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT 'api权限',
                            `storage_id` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '存储空间id',
                            `repository_id` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '仓库id',
                            `path` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '路径',
                            `create_by` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT '' COMMENT '创建人',
                            `create_time` timestamp NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
                            PRIMARY KEY (`id`),
                            UNIQUE KEY `resource_api_authoritie_IDX` (`api_authoritie`) USING BTREE,
                            UNIQUE KEY `resource_storage_id_IDX` (`storage_id`,`repository_id`,`path`) USING BTREE
) ENGINE=InnoDB AUTO_INCREMENT=1000 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='资源表';


DROP TABLE IF EXISTS `role_resource_ref`;
-- folib_scanner.role_resource_ref definition

CREATE TABLE `role_resource_ref` (
                                     `id` bigint NOT NULL AUTO_INCREMENT COMMENT '主键',
                                     `role_id` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '角色id',
                                     `entity_id` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '对象id',
                                     `ref_type` varchar(255) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '关联类型[用户、用户组];[1-用户id、2-用户组id]',
                                     `resource_id` varchar(255) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '资源id;[1-api、2-存储空间、3-仓库、4-路径]',
                                     `storage_privilege` varchar(255) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '存储空间权限',
                                     `repository_privilege` varchar(255) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '仓库权限',
                                     `path_privilege` varchar(255) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '路径权限',
                                     `create_by` varchar(32) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '创建人',
                                     `create_time` timestamp NULL DEFAULT NULL COMMENT '创建时间',
                                     `resource_type` varchar(10) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '资源id;[1-api、2-存储空间、3-仓库、4-路径]',
                                     PRIMARY KEY (`id`),
                                     KEY `role_resource_ref_role_id_IDX` (`role_id`,`entity_id`,`ref_type`) USING BTREE
) ENGINE=InnoDB AUTO_INCREMENT=1000 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='权限表';

DROP TABLE IF EXISTS `user_group`;
-- folib_scanner.user_group definition

CREATE TABLE `user_group` (
                              `id` bigint NOT NULL AUTO_INCREMENT COMMENT '主键',
                              `group_name` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '组名称',
                              `description` varchar(255) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '描述',
                              `join_group` varchar(1) COLLATE utf8mb4_unicode_ci DEFAULT '0' COMMENT '新建用户是否自动加入此用户组',
                              `is_default` varchar(1) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '是否默认',
                              `deleted` varchar(1) COLLATE utf8mb4_unicode_ci DEFAULT '0' COMMENT '是否删除',
                              `create_by` varchar(32) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '创建人',
                              `create_time` timestamp NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
                              `update_by` varchar(32) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '更新人',
                              `update_time` timestamp NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
                              PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=1000 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='用户组';

DROP TABLE IF EXISTS `user_group_ref`;
-- folib_scanner.user_group_ref definition

CREATE TABLE `user_group_ref` (
                                  `id` bigint NOT NULL AUTO_INCREMENT COMMENT '主键',
                                  `user_group_id` bigint DEFAULT NULL COMMENT '用户组id',
                                  `user_id` varchar(100) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '角色id',
                                  `create_by` varchar(32) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '创建人',
                                  `create_time` datetime DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
                                  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=1000 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='用户组用户关联表';

INSERT INTO folib_scanner.user_group (id, group_name, description, join_group, is_default, deleted, create_by, create_time, update_by, update_time) VALUES(1, 'default_group', '默认用户组', '1', '1', '0', NULL, now(), NULL, now());

-- 开始事务
START TRANSACTION;

-- 插入到 resource 表
INSERT INTO folib_scanner.resource (api_authoritie, storage_id, repository_id, `path`, create_by, create_time)
VALUES ('DELETE_USER_GROUP', NULL, NULL, NULL, NULL, NULL);
-- 获取刚插入的 resource 的 id
SET @resource_id = LAST_INSERT_ID();
-- 插入到 role_resource_ref 表
INSERT INTO folib_scanner.role_resource_ref (role_id, entity_id, ref_type, resource_id, storage_privilege, repository_privilege, path_privilege, create_by, create_time, resource_type)
VALUES ('USER_MANAGER', NULL, NULL, @resource_id, NULL, NULL, NULL, NULL, NULL, '1');

-- 插入到 resource 表
INSERT INTO folib_scanner.resource (api_authoritie, storage_id, repository_id, `path`, create_by, create_time)
VALUES ('VIEW_USER_GROUP', NULL, NULL, NULL, NULL, NULL);
-- 获取刚插入的 resource 的 id
SET @resource_id = LAST_INSERT_ID();
-- 插入到 role_resource_ref 表
INSERT INTO folib_scanner.role_resource_ref (role_id, entity_id, ref_type, resource_id, storage_privilege, repository_privilege, path_privilege, create_by, create_time, resource_type)
VALUES ('USER_MANAGER', NULL, NULL, @resource_id, NULL, NULL, NULL, NULL, NULL, '1');

-- 插入到 resource 表
INSERT INTO folib_scanner.resource (api_authoritie, storage_id, repository_id, `path`, create_by, create_time)
VALUES ('UPDATE_USER_GROUP', NULL, NULL, NULL, NULL, NULL);
-- 获取刚插入的 resource 的 id
SET @resource_id = LAST_INSERT_ID();
-- 插入到 role_resource_ref 表
INSERT INTO folib_scanner.role_resource_ref (role_id, entity_id, ref_type, resource_id, storage_privilege, repository_privilege, path_privilege, create_by, create_time, resource_type)
VALUES ('USER_MANAGER', NULL, NULL, @resource_id, NULL, NULL, NULL, NULL, NULL, '1');

-- 插入到 resource 表
INSERT INTO folib_scanner.resource (api_authoritie, storage_id, repository_id, `path`, create_by, create_time)
VALUES ('CREATE_USER_GROUP', NULL, NULL, NULL, NULL, NULL);
-- 获取刚插入的 resource 的 id
SET @resource_id = LAST_INSERT_ID();
-- 插入到 role_resource_ref 表
INSERT INTO folib_scanner.role_resource_ref (role_id, entity_id, ref_type, resource_id, storage_privilege, repository_privilege, path_privilege, create_by, create_time, resource_type)
VALUES ('USER_MANAGER', NULL, NULL, @resource_id, NULL, NULL, NULL, NULL, NULL, '1');
-- 提交事务
COMMIT;


-- 开始事务
START TRANSACTION;

-- 插入到 resource 表
INSERT INTO folib_scanner.resource (api_authoritie, storage_id, repository_id, `path`, create_by, create_time)
VALUES ('DELETE_ROLE', NULL, NULL, NULL, NULL, NULL);
-- 获取刚插入的 resource 的 id
SET @resource_id = LAST_INSERT_ID();
-- 插入到 role_resource_ref 表
INSERT INTO folib_scanner.role_resource_ref (role_id, entity_id, ref_type, resource_id, storage_privilege, repository_privilege, path_privilege, create_by, create_time, resource_type)
VALUES ('USER_MANAGER', NULL, NULL, @resource_id, NULL, NULL, NULL, NULL, NULL, '1');

-- 插入到 resource 表
INSERT INTO folib_scanner.resource (api_authoritie, storage_id, repository_id, `path`, create_by, create_time)
VALUES ('CREATE_ROLE', NULL, NULL, NULL, NULL, NULL);
-- 获取刚插入的 resource 的 id
SET @resource_id = LAST_INSERT_ID();
-- 插入到 role_resource_ref 表
INSERT INTO folib_scanner.role_resource_ref (role_id, entity_id, ref_type, resource_id, storage_privilege, repository_privilege, path_privilege, create_by, create_time, resource_type)
VALUES ('USER_MANAGER', NULL, NULL, @resource_id, NULL, NULL, NULL, NULL, NULL, '1');

-- 插入到 resource 表
INSERT INTO folib_scanner.resource (api_authoritie, storage_id, repository_id, `path`, create_by, create_time)
VALUES ('VIEW_ROLE', NULL, NULL, NULL, NULL, NULL);
-- 获取刚插入的 resource 的 id
SET @resource_id = LAST_INSERT_ID();
-- 插入到 role_resource_ref 表
INSERT INTO folib_scanner.role_resource_ref (role_id, entity_id, ref_type, resource_id, storage_privilege, repository_privilege, path_privilege, create_by, create_time, resource_type)
VALUES ('USER_MANAGER', NULL, NULL, @resource_id, NULL, NULL, NULL, NULL, NULL, '1');

-- 插入到 resource 表
INSERT INTO folib_scanner.resource (api_authoritie, storage_id, repository_id, `path`, create_by, create_time)
VALUES ('UPDATE_ROLE', NULL, NULL, NULL, NULL, NULL);
-- 获取刚插入的 resource 的 id
SET @resource_id = LAST_INSERT_ID();
-- 插入到 role_resource_ref 表
INSERT INTO folib_scanner.role_resource_ref (role_id, entity_id, ref_type, resource_id, storage_privilege, repository_privilege, path_privilege, create_by, create_time, resource_type)
VALUES ('USER_MANAGER', NULL, NULL, @resource_id, NULL, NULL, NULL, NULL, NULL, '1');
-- 提交事务
COMMIT;
