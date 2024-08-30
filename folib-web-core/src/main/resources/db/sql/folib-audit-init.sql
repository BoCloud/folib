DROP TABLE IF EXISTS `audit_log_record`;

CREATE TABLE audit_log_record
(
    id          BIGINT auto_increment NOT NULL PRIMARY KEY,
    module      varchar(50)           NULL COMMENT '事件模块',
    module_name  varchar(50)           NULL COMMENT '模块名称',
    `name`      varchar(20)           NULL COMMENT '事件名称',
    event_name   varchar(50)           NULL COMMENT '事件中文名',
    username    varchar(50)           NULL COMMENT '操作人',
    target      varchar(100)          NULL COMMENT '事件对象',
    `result`    INT                   NULL COMMENT '事件结果',
    request     TEXT                  NULL COMMENT '请求报文',
    response    TEXT                  NULL COMMENT '响应报文',
    create_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间'
)
    ENGINE = InnoDB
    CHARSET = utf8mb4
    COLLATE = utf8mb4_bin COMMENT ='审计日志表';

DROP TABLE IF EXISTS `audit_event`;
CREATE TABLE audit_event
(
    id           BIGINT auto_increment NOT NULL PRIMARY KEY,
    module_value varchar(50)           NULL COMMENT '事件模块',
    module_name  varchar(50)           NULL COMMENT '事件名称',
    event_value  varchar(50)           NULL COMMENT '事件名',
    event_name   varchar(50)           NULL COMMENT '事件中文名',
    used      INT DEFAULT 1 COMMENT '是否被使用1-使用，0-不使用'
)
    ENGINE = InnoDB
    DEFAULT CHARSET = utf8mb4
    COLLATE = utf8mb4_bin COMMENT ='审计事件';

INSERT INTO audit_event (module_value, module_name, event_value,event_name,used) VALUES ('ARTIFACT_REPOSITORY', '制品仓库', 'ADD_STORAGE','新建存储空间',1);
INSERT INTO audit_event (module_value, module_name, event_value,event_name,used) VALUES ('ARTIFACT_REPOSITORY', '制品仓库', 'DELETE_STORAGE','删除存储空间',1);
INSERT INTO audit_event (module_value, module_name, event_value,event_name,used) VALUES ('ARTIFACT_REPOSITORY', '制品仓库', 'ADD_REPOSITORY','新建仓库',1);
INSERT INTO audit_event (module_value, module_name, event_value,event_name,used) VALUES ('ARTIFACT_REPOSITORY', '制品仓库', 'DELETE_REPOSITORY','删除仓库',1);
INSERT INTO audit_event (module_value, module_name, event_value,event_name,used) VALUES ('ARTIFACT_REPOSITORY', '制品仓库', 'PERMIT_REPOSITORY','仓库权限',1);
INSERT INTO audit_event (module_value, module_name, event_value,event_name,used) VALUES ('ARTIFACT_REPOSITORY', '制品仓库', 'CRON_REPOSITORY','仓库定时策略',1);
INSERT INTO audit_event (module_value, module_name, event_value,event_name,used) VALUES ('ARTIFACT_REPOSITORY', '制品仓库', 'UNION_REPOSITORY','联邦仓库',1);
INSERT INTO audit_event (module_value, module_name, event_value,event_name,used) VALUES ('ARTIFACT_REPOSITORY', '制品仓库', 'SCAN_ARTIfFACT','制品扫描',1);
INSERT INTO audit_event (module_value, module_name, event_value,event_name,used) VALUES ('ARTIFACT_REPOSITORY', '制品仓库', 'DELETE_ARTIfFACT','删除制品',1);
INSERT INTO audit_event (module_value, module_name, event_value,event_name,used) VALUES ('ARTIFACT_REPOSITORY', '制品仓库', 'UPLOAD_ARTIfFACT','制品上传',1);


INSERT INTO audit_event (module_value, module_name, event_value,event_name,used) VALUES ('USER_MANAGEMENT', '用户管理', 'CREATE_USER','新增用户',1);
INSERT INTO audit_event (module_value, module_name, event_value,event_name,used) VALUES ('USER_MANAGEMENT', '用户管理', 'DELETE_USER','删除用户',1);
INSERT INTO audit_event (module_value, module_name, event_value,event_name,used) VALUES ('USER_MANAGEMENT', '用户管理', 'USER_GROUP','用户组',1);
INSERT INTO audit_event (module_value, module_name, event_value,event_name,used) VALUES ('USER_MANAGEMENT', '用户管理', 'USER_MANAGEMENT','用户权限',1);
INSERT INTO audit_event (module_value, module_name, event_value,event_name,used) VALUES ('USER_MANAGEMENT', '用户管理', 'WEB_LOGIN','web登录',1);
INSERT INTO audit_event (module_value, module_name, event_value,event_name,used) VALUES ('USER_MANAGEMENT', '用户管理', 'OSS_LOGIN','oss登录',1);


INSERT INTO audit_event (module_value, module_name, event_value,event_name,used) VALUES ('SYSTEM_SETTING', '系统设置', 'BASE_SETTING','基础信息配置',1);
INSERT INTO audit_event (module_value, module_name, event_value,event_name,used) VALUES ('SYSTEM_SETTING', '系统设置', 'SAFE_STRATEGY','安全策略',1);
INSERT INTO audit_event (module_value, module_name, event_value,event_name,used) VALUES ('SYSTEM_SETTING', '系统设置', 'LDAP_SETTING','LDAP配置',1);
INSERT INTO audit_event (module_value, module_name, event_value,event_name,used) VALUES ('SYSTEM_SETTING', '系统设置', 'SSO_SETTING','SSO配置',1);
INSERT INTO audit_event (module_value, module_name, event_value,event_name,used) VALUES ('SYSTEM_SETTING', '系统设置', 'META_SETTING','元数据配置',1);
INSERT INTO audit_event (module_value, module_name, event_value,event_name,used) VALUES ('SYSTEM_SETTING', '系统设置', 'NODE_DISPATCH','节点分发配置',1);
INSERT INTO audit_event (module_value, module_name, event_value,event_name,used) VALUES ('SYSTEM_SETTING', '系统设置', 'WEB_HOOK','webhook',1);
INSERT INTO audit_event (module_value, module_name, event_value,event_name,used) VALUES ('SYSTEM_SETTING', '系统设置', 'UPDATE_META','更新元数据',1);


INSERT INTO audit_event (module_value, module_name, event_value,event_name,used) VALUES ('ADVANCE_SETTING', '高级运维', 'BUILD_GRAPH_INDEX','构建数据',1);
INSERT INTO audit_event (module_value, module_name, event_value,event_name,used) VALUES ('ADVANCE_SETTING', '高级运维', 'MAVEN_INDEXER','Maven数据迁移',1);
INSERT INTO audit_event (module_value, module_name, event_value,event_name,used) VALUES ('ADVANCE_SETTING', '高级运维', 'BUG_UPDATE','漏洞更新',1);
INSERT INTO audit_event (module_value, module_name, event_value,event_name,used) VALUES ('ADVANCE_SETTING', '高级运维', 'BACKUP_STRATEGY','备份策略',1);
INSERT INTO audit_event (module_value, module_name, event_value,event_name,used) VALUES ('ADVANCE_SETTING', '高级运维', 'CACHE_STRATEGY','缓存策略',1);
INSERT INTO audit_event (module_value, module_name, event_value,event_name,used) VALUES ('ADVANCE_SETTING', '高级运维', 'REINDEX','重建索引',1);
INSERT INTO audit_event (module_value, module_name, event_value,event_name,used) VALUES ('ADVANCE_SETTING', '高级运维', 'REGISTER_INDEX','注册索引',1);
INSERT INTO audit_event (module_value, module_name, event_value,event_name,used) VALUES ('ADVANCE_SETTING', '高级运维', 'DELETE_INSTANCE','删除实例',1);




