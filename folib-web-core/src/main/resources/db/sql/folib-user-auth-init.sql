INSERT INTO user_group (id, group_name, description, join_group, is_default, deleted, create_by, create_time, update_by, update_time) VALUES(1, 'GENERAL_GROUP', '默认普通用户组', '1', '1', '0', NULL, now(), NULL, now());
INSERT INTO role_resource_ref (role_id, entity_id, ref_type, resource_id, storage_privilege, repository_privilege, path_privilege, create_by, create_time, resource_type) VALUES('GENERAL', '1', '2', NULL, NULL, NULL, NULL, NULL, now(), NULL);



-- 插入到 resource 表
INSERT INTO resource (id, api_authoritie, storage_id, repository_id, `path`, create_by, create_time)
VALUES ('DELETE_USER_GROUP', 'DELETE_USER_GROUP', NULL, NULL, NULL, NULL, now());
-- 插入到 role_resource_ref 表
INSERT INTO role_resource_ref (role_id, entity_id, ref_type, resource_id, storage_privilege, repository_privilege, path_privilege, create_by, create_time, resource_type)
VALUES ('USER_MANAGER', NULL, NULL, 'DELETE_USER_GROUP', NULL, NULL, NULL, NULL, now(), '1');

-- 插入到 resource 表
INSERT INTO resource (id, api_authoritie, storage_id, repository_id, `path`, create_by, create_time)
VALUES ('VIEW_USER_GROUP','VIEW_USER_GROUP', NULL, NULL, NULL, NULL, now());
-- 插入到 role_resource_ref 表
INSERT INTO role_resource_ref (role_id, entity_id, ref_type, resource_id, storage_privilege, repository_privilege, path_privilege, create_by, create_time, resource_type)
VALUES ('USER_MANAGER', NULL, NULL, 'VIEW_USER_GROUP', NULL, NULL, NULL, NULL, now(), '1');

-- 插入到 resource 表
INSERT INTO resource (id,api_authoritie, storage_id, repository_id, `path`, create_by, create_time)
VALUES ('UPDATE_USER_GROUP','UPDATE_USER_GROUP', NULL, NULL, NULL, NULL, now());
-- 插入到 role_resource_ref 表
INSERT INTO role_resource_ref (role_id, entity_id, ref_type, resource_id, storage_privilege, repository_privilege, path_privilege, create_by, create_time, resource_type)
VALUES ('USER_MANAGER', NULL, NULL, 'UPDATE_USER_GROUP', NULL, NULL, NULL, NULL, now(), '1');

-- 插入到 resource 表
INSERT INTO resource (id,api_authoritie, storage_id, repository_id, `path`, create_by, create_time)
VALUES ('CREATE_USER_GROUP','CREATE_USER_GROUP', NULL, NULL, NULL, NULL, now());
-- 插入到 role_resource_ref 表
INSERT INTO role_resource_ref (role_id, entity_id, ref_type, resource_id, storage_privilege, repository_privilege, path_privilege, create_by, create_time, resource_type)
VALUES ('USER_MANAGER', NULL, NULL, 'CREATE_USER_GROUP', NULL, NULL, NULL, NULL, now(), '1');



-- 插入到 resource 表
INSERT INTO resource (id,api_authoritie, storage_id, repository_id, `path`, create_by, create_time)
VALUES ('DELETE_ROLE','DELETE_ROLE', NULL, NULL, NULL, NULL, now());
-- 插入到 role_resource_ref 表
INSERT INTO role_resource_ref (role_id, entity_id, ref_type, resource_id, storage_privilege, repository_privilege, path_privilege, create_by, create_time, resource_type)
VALUES ('USER_MANAGER', NULL, NULL, 'DELETE_ROLE', NULL, NULL, NULL, NULL, now(), '1');

-- 插入到 resource 表
INSERT INTO resource (id,api_authoritie, storage_id, repository_id, `path`, create_by, create_time)
VALUES ('CREATE_ROLE','CREATE_ROLE', NULL, NULL, NULL, NULL, now());
-- 插入到 role_resource_ref 表
INSERT INTO role_resource_ref (role_id, entity_id, ref_type, resource_id, storage_privilege, repository_privilege, path_privilege, create_by, create_time, resource_type)
VALUES ('USER_MANAGER', NULL, NULL, 'CREATE_ROLE', NULL, NULL, NULL, NULL, now(), '1');

-- 插入到 resource 表
INSERT INTO resource (id,api_authoritie, storage_id, repository_id, `path`, create_by, create_time)
VALUES ('VIEW_ROLE','VIEW_ROLE', NULL, NULL, NULL, NULL, now());
-- 插入到 role_resource_ref 表
INSERT INTO role_resource_ref (role_id, entity_id, ref_type, resource_id, storage_privilege, repository_privilege, path_privilege, create_by, create_time, resource_type)
VALUES ('USER_MANAGER', NULL, NULL, 'VIEW_ROLE', NULL, NULL, NULL, NULL, now(), '1');

-- 插入到 resource 表
INSERT INTO resource (id,api_authoritie, storage_id, repository_id, `path`, create_by, create_time)
VALUES ('UPDATE_ROLE','UPDATE_ROLE', NULL, NULL, NULL, NULL, now());
-- 插入到 role_resource_ref 表
INSERT INTO role_resource_ref (role_id, entity_id, ref_type, resource_id, storage_privilege, repository_privilege, path_privilege, create_by, create_time, resource_type)
VALUES ('USER_MANAGER', NULL, NULL, 'UPDATE_ROLE', NULL, NULL, NULL, NULL, now(), '1');

INSERT INTO folib_role (id, cn_name, en_name, description, deleted, is_default, create_by, create_time, update_by, update_time) VALUES('READERS', '全局只读角色', 'READERS', '全局只读角色，可查看平台所有仓库', '0', '1', NULL, now(), NULL, now());
INSERT INTO role_resource_ref (role_id, entity_id, ref_type, resource_id, storage_privilege, repository_privilege, path_privilege, create_by, create_time, resource_type) VALUES('READERS', NULL, NULL, 'ARTIFACTS_VIEW', NULL, NULL, NULL, NULL, now(), '1');
INSERT INTO role_resource_ref (role_id, entity_id, ref_type, resource_id, storage_privilege, repository_privilege, path_privilege, create_by, create_time, resource_type) VALUES('READERS', NULL, NULL, 'ARTIFACTS_RESOLVE', NULL, NULL, NULL, NULL, now(), '1');