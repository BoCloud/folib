SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

-- ----------------------------
-- Table structure for access_token
-- ----------------------------
DROP TABLE IF EXISTS access_token;
CREATE TABLE access_token (
                              id int(11) NOT NULL AUTO_INCREMENT,
                              token_id varchar(50) DEFAULT NULL COMMENT '令牌标识',
                              description varchar(200) DEFAULT NULL COMMENT '描述',
                              operator varchar(100) DEFAULT NULL COMMENT '操作人',
                              expire_time timestamp NULL DEFAULT NULL COMMENT '过期时间',
                              username varchar(50) DEFAULT NULL COMMENT '用户',
                              create_time timestamp NOT NULL DEFAULT current_timestamp() COMMENT '创建时间',
                              PRIMARY KEY (id),
                              UNIQUE KEY unique_token (token_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci COMMENT='访问令牌表';




-- ----------------------------
-- Table structure for dict
-- ----------------------------
DROP TABLE IF EXISTS dict;
CREATE TABLE dict (
                      id bigint(20) NOT NULL AUTO_INCREMENT COMMENT '主键ID',
                      create_time datetime DEFAULT current_timestamp() COMMENT '创建时间',
                      dict_type varchar(255) DEFAULT NULL,
                      dict_key varchar(255) DEFAULT NULL,
                      dict_value varchar(512) DEFAULT NULL,
                      alias text DEFAULT NULL,
                      comment varchar(255) DEFAULT '' COMMENT '备注',
                      PRIMARY KEY (id),
                      KEY idx_dict_key (dict_key) USING BTREE,
                      KEY idx_dict_type (dict_type) USING BTREE,
                      KEY idx_dict_type_dict_key (dict_type,dict_key)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci COMMENT='字典';

-- ----------------------------
-- Table structure for folib_lock
-- ----------------------------
DROP TABLE IF EXISTS folib_lock;
CREATE TABLE folib_lock (
                            name varchar(64) NOT NULL,
                            lock_until timestamp(3) NOT NULL DEFAULT current_timestamp(3) ON UPDATE current_timestamp(3),
                            locked_at timestamp(3) NOT NULL DEFAULT current_timestamp(3),
                            locked_by varchar(255) NOT NULL,
                            PRIMARY KEY (name) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

-- ----------------------------
-- Table structure for folib_role
-- ----------------------------
DROP TABLE IF EXISTS folib_role;
CREATE TABLE folib_role (
                            id varchar(100) NOT NULL COMMENT '主键',
                            cn_name varchar(255) DEFAULT NULL COMMENT '中文名称',
                            en_name varchar(255) DEFAULT NULL COMMENT '英文名称',
                            description varchar(255) DEFAULT NULL COMMENT '角色描述',
                            deleted varchar(1) DEFAULT '0' COMMENT '是否删除',
                            is_default varchar(1) DEFAULT '0' COMMENT '是否默认',
                            create_by varchar(255) DEFAULT '' COMMENT '创建人',
                            create_time timestamp NULL DEFAULT current_timestamp() COMMENT '创建时间',
                            update_by varchar(255) DEFAULT '' COMMENT '更新人',
                            update_time timestamp NULL DEFAULT current_timestamp() ON UPDATE current_timestamp() COMMENT '更新时间',
                            PRIMARY KEY (id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci COMMENT='角色信息';

-- ----------------------------
-- Table structure for folib_scanner
-- ----------------------------
DROP TABLE IF EXISTS folib_scanner;
CREATE TABLE folib_scanner (
                               path varchar(255) NOT NULL COMMENT '制品路径',
                               repository varchar(255) NOT NULL COMMENT '仓库名称',
                               storage varchar(255) NOT NULL COMMENT '存储空间',
                               artifact_path varchar(255) DEFAULT NULL,
                               report longtext DEFAULT NULL COMMENT '报告',
                               file_type varchar(10) DEFAULT NULL COMMENT '文件类型',
                               scan_status varchar(10) DEFAULT NULL COMMENT '扫描状态',
                               on_scan tinyint(1) DEFAULT NULL COMMENT '是否扫描',
                               scan_time datetime DEFAULT NULL COMMENT '扫描时间',
                               level varchar(32) DEFAULT NULL COMMENT '安全级别',
                               dependency_count int(11) DEFAULT NULL COMMENT '依赖数量',
                               vulnerable_count int(11) DEFAULT NULL COMMENT '容易被攻击的依赖数量',
                               vulnerabilites_count int(11) DEFAULT NULL COMMENT '漏洞数量',
                               suppressed_count int(11) DEFAULT NULL COMMENT '被封存的数量',
                               cve_checked_time datetime DEFAULT NULL,
                               cve_update_time datetime DEFAULT NULL,
                               PRIMARY KEY (path) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

-- ----------------------------
-- Table structure for folib_user
-- ----------------------------
DROP TABLE IF EXISTS folib_user;
CREATE TABLE folib_user (
                            id varchar(100) NOT NULL COMMENT '主键',
                            username varchar(255) DEFAULT NULL COMMENT '用户名',
                            password varchar(255) DEFAULT NULL COMMENT '密码',
                            original_password varchar(255) DEFAULT NULL COMMENT '原始密码',
                            avatar varchar(255) DEFAULT NULL COMMENT '头像',
                            email varchar(64) DEFAULT NULL COMMENT '邮件',
                            user_type varchar(20) DEFAULT NULL COMMENT '用户类型',
                            enabled varchar(20) DEFAULT 'true' COMMENT '是否启用',
                            source_id varchar(255) DEFAULT NULL COMMENT '来源',
                            deleted tinyint(1) DEFAULT 0 COMMENT '是否删除',
                            create_by varchar(255) DEFAULT '' COMMENT '创建人',
                            create_time timestamp NULL DEFAULT current_timestamp() COMMENT '创建时间',
                            update_by varchar(255) DEFAULT '' COMMENT '更新人',
                            update_time timestamp NULL DEFAULT current_timestamp() ON UPDATE current_timestamp() COMMENT '更新时间',
                            nickname varchar(255) DEFAULT NULL COMMENT '用户昵称',
                            PRIMARY KEY (id),
                            KEY idx_user_type (user_type)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci COMMENT='用户信息';

-- ----------------------------
-- Table structure for git_lfs_locks
-- ----------------------------
DROP TABLE IF EXISTS git_lfs_locks;
CREATE TABLE git_lfs_locks (
                               id varchar(255) NOT NULL COMMENT 'id',
                               storage_id varchar(255) NOT NULL COMMENT '存储ID',
                               repository_id varchar(255) NOT NULL COMMENT '仓库ID',
                               path varchar(255) NOT NULL COMMENT '锁定文件的路径',
                               locked_at bigint(20) NOT NULL COMMENT '创建锁的时间戳',
                               owner varchar(255) NOT NULL COMMENT '所属人',
                               ref varchar(255) NOT NULL COMMENT '描述锁所属的服务器引用',
                               PRIMARY KEY (id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci COMMENT='git lfs 锁表';

-- ----------------------------
-- Table structure for license
-- ----------------------------
DROP TABLE IF EXISTS license;
CREATE TABLE license (
                         id bigint(20) NOT NULL AUTO_INCREMENT COMMENT '主键ID',
                         create_time datetime DEFAULT current_timestamp() COMMENT '创建时间',
                         license_id varchar(255) DEFAULT '' COMMENT '许可证id',
                         license_name varchar(255) DEFAULT '' COMMENT '许可证名称',
                         license_url text DEFAULT NULL COMMENT '许可证地址',
                         is_custom_license tinyint(3) DEFAULT 0 COMMENT '是否是自定义许可证 1是 0否',
                         is_deprecated tinyint(3) DEFAULT 0 COMMENT '是否已弃用 1是 0否',
                         is_osi_approved tinyint(3) DEFAULT 0 COMMENT '是否属于OSI-Approved授权协议 1是 0否',
                         is_fsf_libre tinyint(3) DEFAULT 0 COMMENT '是否为自由软件基金会 1是 0否',
                         header text DEFAULT NULL COMMENT '许可证头信息',
                         template text DEFAULT NULL COMMENT '许可证模板',
                         content text DEFAULT NULL COMMENT '许可证原文内容',
                         content_cn text DEFAULT NULL COMMENT '许可证中文内容',
                         comment text DEFAULT NULL COMMENT '备注',
                         black_white_type int(11) DEFAULT 0 COMMENT '黑白名单类型 1 白名单 2 黑名单',
                         PRIMARY KEY (id),
                         KEY idx_license_id (license_id) USING BTREE,
                         KEY idx_license_name (license_name) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci COMMENT='许可证';

-- ----------------------------
-- Table structure for migrate_info
-- ----------------------------
DROP TABLE IF EXISTS migrate_info;
CREATE TABLE migrate_info (
                              id bigint(20) NOT NULL AUTO_INCREMENT COMMENT 'id',
                              storage_id varchar(255) DEFAULT NULL COMMENT '存储空间',
                              repository_id varchar(100) DEFAULT NULL COMMENT '仓库',
                              migrate_id varchar(100) DEFAULT NULL COMMENT '迁移id',
                              layout varchar(50) DEFAULT NULL COMMENT '布局',
                              migrate_type varchar(255) DEFAULT NULL COMMENT 'jfrog',
                              sync_status int(11) DEFAULT NULL COMMENT '迁移状态',
                              total_artifact int(11) DEFAULT NULL COMMENT '制品总数',
                              success_mount int(11) DEFAULT NULL COMMENT '迁移成功量',
                              sync_property int(11) DEFAULT NULL COMMENT '是否迁移元数据',
                              sync_dir_path varchar(255) DEFAULT NULL COMMENT '同步文件路径',
                              used_space varchar(255) DEFAULT NULL COMMENT '原制品大小',
                              post_layout varchar(100) DEFAULT NULL COMMENT '修改后的布局',
                              index_finish tinyint(1) DEFAULT 0 COMMENT '索引是否完成',
                              PRIMARY KEY (id),
                              KEY idx_migrate_id (migrate_id) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci COMMENT='迁移数据表';

-- ----------------------------
-- Table structure for properties
-- ----------------------------
DROP TABLE IF EXISTS properties;
CREATE TABLE properties (
                            id varchar(50) NOT NULL,
                            value varchar(500) DEFAULT NULL,
                            PRIMARY KEY (id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;


-- ----------------------------
-- Table structure for resource
-- ----------------------------
DROP TABLE IF EXISTS resource;
CREATE TABLE resource (
                          id varchar(255) NOT NULL,
                          api_authoritie varchar(100) DEFAULT NULL COMMENT 'api权限',
                          storage_id varchar(255) DEFAULT NULL COMMENT '存储空间id',
                          repository_id varchar(255) DEFAULT NULL COMMENT '仓库id',
                          path varchar(255) DEFAULT NULL COMMENT '路径',
                          create_by varchar(255) DEFAULT '' COMMENT '创建人',
                          create_time timestamp NULL DEFAULT current_timestamp() COMMENT '创建时间',
                          PRIMARY KEY (id),
                          UNIQUE KEY resource_api_authoritie_IDX (api_authoritie) USING BTREE,
                          UNIQUE KEY resource_storage_id_IDX (storage_id,repository_id,path) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci COMMENT='资源表';

-- ----------------------------
-- Table structure for role_resource_ref
-- ----------------------------
DROP TABLE IF EXISTS role_resource_ref;
CREATE TABLE role_resource_ref (
                                   id bigint(20) NOT NULL AUTO_INCREMENT COMMENT '主键',
                                   role_id varchar(255) NOT NULL COMMENT '角色id',
                                   entity_id varchar(255) DEFAULT NULL COMMENT '对象id',
                                   ref_type int(11) DEFAULT NULL COMMENT '关联类型[用户、用户组];[1-用户id、2-用户组id]',
                                   resource_id varchar(255) DEFAULT NULL COMMENT '资源id;[1-api、2-存储空间、3-仓库、4-路径]',
                                   storage_privilege varchar(255) DEFAULT NULL COMMENT '存储空间权限',
                                   repository_privilege varchar(255) DEFAULT NULL COMMENT '仓库权限',
                                   path_privilege varchar(255) DEFAULT NULL COMMENT '路径权限',
                                   create_by varchar(32) DEFAULT NULL COMMENT '创建人',
                                   create_time timestamp NULL DEFAULT current_timestamp() COMMENT '创建时间',
                                   resource_type varchar(10) DEFAULT NULL COMMENT '资源id;[1-api、2-存储空间、3-仓库、4-路径]',
                                   PRIMARY KEY (id),
                                   KEY role_resource_ref_role_id_IDX (role_id,entity_id,ref_type) USING BTREE,
                                   KEY idx_ref_type_entity_id (ref_type,entity_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci COMMENT='权限表';

-- ----------------------------
-- Table structure for scan_rules
-- ----------------------------
DROP TABLE IF EXISTS scan_rules;
CREATE TABLE scan_rules (
                            id varchar(255) NOT NULL,
                            repository varchar(255) DEFAULT NULL COMMENT '仓库名称',
                            storage varchar(255) DEFAULT NULL COMMENT '存储空间',
                            layout varchar(255) DEFAULT NULL COMMENT '仓库类型',
                            on_scan tinyint(1) DEFAULT NULL COMMENT '是否扫描',
                            bom_on_scan tinyint(1) DEFAULT 0 COMMENT 'bom扫描',
                            project_uuid varchar(255) DEFAULT '' COMMENT '父项目uuid',
                            scan_rule varchar(255) DEFAULT NULL COMMENT '扫描规则',
                            PRIMARY KEY (id) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

-- ----------------------------
-- Table structure for user_group
-- ----------------------------
DROP TABLE IF EXISTS user_group;
CREATE TABLE user_group (
                            id bigint(20) NOT NULL AUTO_INCREMENT COMMENT '主键',
                            group_name varchar(255) NOT NULL COMMENT '组名称',
                            description varchar(255) DEFAULT NULL COMMENT '描述',
                            join_group varchar(1) DEFAULT '0' COMMENT '新建用户是否自动加入此用户组',
                            is_default varchar(1) DEFAULT NULL COMMENT '是否默认',
                            deleted varchar(1) DEFAULT '0' COMMENT '是否删除',
                            create_by varchar(32) DEFAULT NULL COMMENT '创建人',
                            create_time timestamp NULL DEFAULT current_timestamp() COMMENT '创建时间',
                            update_by varchar(32) DEFAULT NULL COMMENT '更新人',
                            update_time timestamp NULL DEFAULT current_timestamp() ON UPDATE current_timestamp() COMMENT '更新时间',
                            PRIMARY KEY (id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci COMMENT='用户组';

-- ----------------------------
-- Table structure for user_group_ref
-- ----------------------------
DROP TABLE IF EXISTS user_group_ref;
CREATE TABLE user_group_ref (
                                id bigint(20) NOT NULL AUTO_INCREMENT COMMENT '主键',
                                user_group_id bigint(20) DEFAULT NULL COMMENT '用户组id',
                                user_group_name varchar(255) DEFAULT NULL COMMENT '用户组名称',
                                user_id varchar(100) DEFAULT NULL COMMENT '用户id',
                                create_by varchar(32) DEFAULT NULL COMMENT '创建人',
                                create_time datetime DEFAULT current_timestamp() COMMENT '创建时间',
                                PRIMARY KEY (id),
                                KEY idx_user_group_id (user_group_id) USING BTREE,
                                KEY idx_user_id (user_id) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci COMMENT='用户组用户关联表';

-- ----------------------------
-- Table structure for webhook_events_log
-- ----------------------------
DROP TABLE IF EXISTS webhook_events_log;
CREATE TABLE webhook_events_log (
                                    id bigint(20) NOT NULL AUTO_INCREMENT COMMENT '主键ID',
                                    event_type varchar(255) DEFAULT '' COMMENT '事件类型',
                                    event_repository_id varchar(255) DEFAULT '' COMMENT '事件仓库名称',
                                    storage_id varchar(255) DEFAULT '' COMMENT '存储空间名称',
                                    repository_id varchar(255) DEFAULT '' COMMENT '仓库名称',
                                    artifact_name varchar(255) DEFAULT '' COMMENT '制品名称',
                                    artifact_path varchar(1000) DEFAULT '' COMMENT '制品路径',
                                    source_artifact_path varchar(1000) DEFAULT '' COMMENT '源路径',
                                    target_artifact_path varchar(1000) DEFAULT '' COMMENT '目标路径',
                                    sha256_checksum varchar(255) DEFAULT '' COMMENT 'sha256的checksum',
                                    size bigint(20) DEFAULT 0 COMMENT '文件大小',
                                    base_url varchar(255) DEFAULT '' COMMENT '访问url前缀',
                                    status int(11) DEFAULT 1 COMMENT '状态 （1：初始状态 2：成功 3：失败）',
                                    failure_reason text DEFAULT NULL COMMENT '失败的原因',
                                    retry int(11) DEFAULT 1 COMMENT '是否重试（0:不重试，1:重试）',
                                    retry_count int(11) DEFAULT 0 COMMENT '重试次数',
                                    retry_time datetime DEFAULT NULL COMMENT '重试时间',
                                    create_by varchar(255) DEFAULT '' COMMENT '创建人',
                                    create_time datetime DEFAULT current_timestamp() COMMENT '创建时间',
                                    update_by varchar(255) DEFAULT '' COMMENT '更新人',
                                    update_time datetime DEFAULT NULL COMMENT '更新时间',
                                    PRIMARY KEY (id),
                                    KEY idx_storage_id (storage_id) USING BTREE,
                                    KEY idx_repository_id (repository_id) USING BTREE,
                                    KEY idx_artifact_name (artifact_name) USING BTREE,
                                    KEY idx_event_type (event_type) USING BTREE,
                                    KEY idx_event_repository_id (event_repository_id) USING BTREE,
                                    KEY idx_status (status) USING BTREE,
                                    KEY idx_retry (retry) USING BTREE,
                                    KEY idx_retry_count (retry_count) USING BTREE,
                                    KEY idx_sha256_checksum (sha256_checksum) USING BTREE,
                                    KEY idx_create_time (create_time) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci COMMENT='webhook事件记录';

-- ----------------------------
-- Table structure for webhook_log
-- ----------------------------
DROP TABLE IF EXISTS webhook_log;
CREATE TABLE webhook_log (
                             id bigint(20) NOT NULL AUTO_INCREMENT COMMENT '主键ID',
                             create_time datetime DEFAULT current_timestamp() COMMENT '创建时间',
                             event_type varchar(255) DEFAULT '' COMMENT '事件类型',
                             storage_id varchar(255) DEFAULT '' COMMENT '存储空间名称',
                             repository_id varchar(255) DEFAULT '' COMMENT '仓库名称',
                             artifact_path varchar(255) DEFAULT '' COMMENT '制品路径',
                             url varchar(255) DEFAULT '' COMMENT '请求url',
                             access_token varchar(255) DEFAULT '' COMMENT '访问令牌',
                             method varchar(255) DEFAULT '' COMMENT '请求方式',
                             completion_time decimal(16,2) DEFAULT 0.00 COMMENT '完成时间（秒）',
                             request_headers text DEFAULT NULL COMMENT '请求头',
                             request longtext DEFAULT NULL COMMENT '请求报文',
                             response_status varchar(255) DEFAULT '200' COMMENT '响应状态码',
                             response_headers text DEFAULT NULL COMMENT '响应头',
                             response longtext DEFAULT NULL COMMENT '响应报文',
                             remark varchar(255) DEFAULT '' COMMENT '备注',
                             PRIMARY KEY (id),
                             KEY idx_storage_id (storage_id) USING BTREE,
                             KEY idx_repository_id (repository_id) USING BTREE,
                             KEY idx_artifact_path (artifact_path) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci COMMENT='webhook';

DROP TABLE IF EXISTS affected_version_attribution;
create table if not exists affected_version_attribution
(
    id                  bigint auto_increment
    primary key,
    first_seen          timestamp    not null default current_timestamp on update current_timestamp,
    `last_seen`           timestamp    not null default current_timestamp,
    source              varchar(255) null,
    uuid                varchar(36)  not null,
    vulnerability       varchar(128) not null,
    vulnerable_software varchar(255) not null,
    constraint affectedversionattribution_pk4
    unique (vulnerability, vulnerable_software),
    constraint affected_version_attribution_pk5
    unique (vulnerable_software, vulnerability, source)
    )engine=innodb  default charset=utf8mb4 collate=utf8mb4_general_ci;

create index affected_version_attribution_vulnerability_index
    on affected_version_attribution (vulnerability);

create index affected_version_attribution_vulnerable_software_index
    on affected_version_attribution (vulnerable_software);


DROP TABLE IF EXISTS cwe;
create table cwe (
                     id bigint not null auto_increment,
                     cweid int not null,
                     name varchar(255) character set utf8mb4 collate utf8mb4_general_ci not null,
                     primary key (id),
                     unique key cwe_cweid_idx (cweid)
) engine=innodb  default charset=utf8mb4 collate=utf8mb4_general_ci;

DROP TABLE IF EXISTS vulnerability;
CREATE TABLE `vulnerability` (
                                 `id` bigint(20) NOT NULL AUTO_INCREMENT,
                                 `created` timestamp NULL DEFAULT NULL,
                                 `credits` mediumtext DEFAULT NULL,
                                 `cvssv2basescore` decimal(19,1) DEFAULT NULL,
                                 `cvssv2exploitscore` decimal(19,1) DEFAULT NULL,
                                 `cvssv2impactscore` decimal(19,1) DEFAULT NULL,
                                 `cvssv2vector` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL,
                                 `cvssv3basescore` decimal(19,1) DEFAULT NULL,
                                 `cvssv3exploitscore` decimal(19,1) DEFAULT NULL,
                                 `cvssv3impactscore` decimal(19,1) DEFAULT NULL,
                                 `cvssv3vector` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL,
                                 `cwes` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL,
                                 `description` mediumtext DEFAULT NULL,
                                 `detail` mediumtext DEFAULT NULL,
                                 `epsspercentile` decimal(19,5) DEFAULT NULL,
                                 `epssscore` decimal(19,5) DEFAULT NULL,
                                 `friendlyvulnid` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL,
                                 `owasprrbusinessimpactscore` decimal(19,1) DEFAULT NULL,
                                 `owasprrlikelihoodscore` decimal(19,1) DEFAULT NULL,
                                 `owasprrtechnicalimpactscore` decimal(19,1) DEFAULT NULL,
                                 `owasprrvector` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL,
                                 `patchedversions` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL,
                                 `published` timestamp NULL DEFAULT NULL,
                                 `recommendation` mediumtext DEFAULT NULL,
                                 `references` mediumtext DEFAULT NULL,
                                 `severity` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL,
                                 `source` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL,
                                 `subtitle` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL,
                                 `title` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL,
                                 `updated` timestamp NULL DEFAULT NULL,
                                 `uuid` varchar(36) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL,
                                 `vulnid` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL,
                                 `vulnerableversions` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL,
                                 `name` mediumtext DEFAULT NULL,
                                 `cnv_id` varchar(128) DEFAULT NULL,
                                 `zh_description` mediumtext DEFAULT NULL,
                                 PRIMARY KEY (`id`),
                                 UNIQUE KEY `vulnerability_u1` (`vulnid`,`source`),
                                 UNIQUE KEY `vulnerability_uuid_idx` (`uuid`),
                                 KEY `vulnerability_vulnid_idx` (`vulnid`),
                                 KEY `vulnerability_published_idx` (`published`),
                                 KEY `vulnerability_updated_idx` (`updated`),
                                 KEY `vulnerability_created_idx` (`created`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

DROP TABLE IF EXISTS vulnerable_software;
create table vulnerable_software (
                                     id bigint not null auto_increment,
                                     cpe22 varchar(255) character set utf8mb4 collate utf8mb4_general_ci default null,
                                     cpe23 varchar(255) character set utf8mb4 collate utf8mb4_general_ci default null,
                                     edition varchar(255) character set utf8mb4 collate utf8mb4_general_ci default null,
                                     language varchar(255) character set utf8mb4 collate utf8mb4_general_ci default null,
                                     other varchar(255) character set utf8mb4 collate utf8mb4_general_ci default null,
                                     part varchar(255) character set utf8mb4 collate utf8mb4_general_ci default null,
                                     product varchar(255) character set utf8mb4 collate utf8mb4_general_ci default null,
                                     purl varchar(255) character set utf8mb4 collate utf8mb4_general_ci default null,
                                     purl_name varchar(128) character set utf8mb4 collate utf8mb4_general_ci default null,
                                     purl_namespace varchar(128) character set utf8mb4 collate utf8mb4_general_ci default null,
                                     purl_qualifiers varchar(255) character set utf8mb4 collate utf8mb4_general_ci default null,
                                     purl_subpath varchar(255) character set utf8mb4 collate utf8mb4_general_ci default null,
                                     purl_type varchar(60) character set utf8mb4 collate utf8mb4_general_ci default null,
                                     purl_version varchar(255) character set utf8mb4 collate utf8mb4_general_ci default null,
                                     swedition varchar(255) character set utf8mb4 collate utf8mb4_general_ci default null,
                                     targethw varchar(255) character set utf8mb4 collate utf8mb4_general_ci default null,
                                     targetsw varchar(255) character set utf8mb4 collate utf8mb4_general_ci default null,
                                     `update` varchar(255) character set utf8mb4 collate utf8mb4_general_ci default null,
                                     uuid varchar(36) character set utf8mb4 collate utf8mb4_general_ci not null,
                                     vendor varchar(255) character set utf8mb4 collate utf8mb4_general_ci default null,
                                     version varchar(255) character set utf8mb4 collate utf8mb4_general_ci default null,
                                     versionendexcluding varchar(255) character set utf8mb4 collate utf8mb4_general_ci default null,
                                     versionendincluding varchar(255) character set utf8mb4 collate utf8mb4_general_ci default null,
                                     versionstartexcluding varchar(255) character set utf8mb4 collate utf8mb4_general_ci default null,
                                     versionstartincluding varchar(255) character set utf8mb4 collate utf8mb4_general_ci default null,
                                     vulnerable bit(1) not null,
                                     primary key (id),
                                     unique key vulnerablesoftware_uuid_idx (uuid),
                                     key vulnerablesoftware_part_vendor_product_idx (part,vendor,product),
                                     key vulnerablesoftware_purl_type_ns_name_idx (purl_type,purl_namespace,purl_name)
) engine=innodb  default charset=utf8mb4 collate=utf8mb4_general_ci;

DROP TABLE IF EXISTS vulnerable_software_vulnerabilities;
create table if not exists vulnerable_software_vulnerabilities
(
    vulnerability_id      varchar(128)  null,
    vulnerablesoftware_id varchar(255) null,
    constraint vulnerable_software_vulnerabilities_pk
    unique (vulnerability_id, vulnerablesoftware_id)
    )engine=innodb default charset=utf8mb4 collate=utf8mb4_general_ci;

SET FOREIGN_KEY_CHECKS = 1;
