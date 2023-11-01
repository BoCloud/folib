DROP PROCEDURE IF EXISTS save_property;
DROP PROCEDURE IF EXISTS update_ecosystems;
DROP PROCEDURE IF EXISTS update_ecosystems2;
DROP PROCEDURE IF EXISTS cleanup_orphans;
DROP PROCEDURE IF EXISTS update_vulnerability;
DROP PROCEDURE IF EXISTS insert_software;
DROP PROCEDURE IF EXISTS merge_ecosystem;
DROP TABLE IF EXISTS software;
DROP TABLE IF EXISTS cpeEntry;
DROP TABLE IF EXISTS `reference`;
DROP TABLE IF EXISTS properties;
DROP TABLE IF EXISTS cweEntry;
DROP TABLE IF EXISTS vulnerability;
DROP TABLE IF EXISTS cpeEcosystemCache;
DROP TABLE IF EXISTS `cluster_datasync_task`;
DROP TABLE IF EXISTS `folib_lock`;
DROP TABLE IF EXISTS `folib_scanner`;
DROP TABLE IF EXISTS `scan_rules`;

CREATE TABLE vulnerability (id int auto_increment PRIMARY KEY, cve VARCHAR(20) UNIQUE,
                            description VARCHAR(8000), v2Severity VARCHAR(20), v2ExploitabilityScore DECIMAL(3,1),
                            v2ImpactScore DECIMAL(3,1), v2AcInsufInfo BOOLEAN, v2ObtainAllPrivilege BOOLEAN,
                            v2ObtainUserPrivilege BOOLEAN, v2ObtainOtherPrivilege BOOLEAN, v2UserInteractionRequired BOOLEAN,
                            v2Score DECIMAL(3,1), v2AccessVector VARCHAR(20), v2AccessComplexity VARCHAR(20),
                            v2Authentication VARCHAR(20), v2ConfidentialityImpact VARCHAR(20), v2IntegrityImpact VARCHAR(20),
                            v2AvailabilityImpact VARCHAR(20), v2Version VARCHAR(5), v3ExploitabilityScore DECIMAL(3,1),
                            v3ImpactScore DECIMAL(3,1), v3AttackVector VARCHAR(20), v3AttackComplexity VARCHAR(20),
                            v3PrivilegesRequired VARCHAR(20), v3UserInteraction VARCHAR(20), v3Scope VARCHAR(20),
                            v3ConfidentialityImpact VARCHAR(20), v3IntegrityImpact VARCHAR(20), v3AvailabilityImpact VARCHAR(20),
                            v3BaseScore DECIMAL(3,1), v3BaseSeverity VARCHAR(20), v3Version VARCHAR(5));

CREATE TABLE `reference` (id INT auto_increment PRIMARY KEY,cveid INT, name VARCHAR(1000), url VARCHAR(1000), source VARCHAR(255),
                          CONSTRAINT fkReference FOREIGN KEY (cveid) REFERENCES vulnerability(id) ON DELETE CASCADE);

CREATE TABLE cpeEntry (id INT auto_increment PRIMARY KEY, part CHAR(1), vendor VARCHAR(255), product VARCHAR(255),
                       version VARCHAR(255), update_version VARCHAR(255), edition VARCHAR(255), lang VARCHAR(20), sw_edition VARCHAR(255),
                       target_sw VARCHAR(255), target_hw VARCHAR(255), other VARCHAR(255), ecosystem VARCHAR(255));

CREATE TABLE software (id INT auto_increment PRIMARY KEY,cveid INT, cpeEntryId INT, versionEndExcluding VARCHAR(60), versionEndIncluding VARCHAR(60),
                       versionStartExcluding VARCHAR(60), versionStartIncluding VARCHAR(60), vulnerable BOOLEAN
    , CONSTRAINT fkSoftwareCve FOREIGN KEY (cveid) REFERENCES vulnerability(id) ON DELETE CASCADE
    , CONSTRAINT fkSoftwareCpeProduct FOREIGN KEY (cpeEntryId) REFERENCES cpeEntry(id));

CREATE TABLE cweEntry (id INT auto_increment PRIMARY KEY,cveid INT, cwe VARCHAR(20),
                       CONSTRAINT fkCweEntry FOREIGN KEY (cveid) REFERENCES vulnerability(id) ON DELETE CASCADE);

CREATE TABLE cpeEcosystemCache (vendor VARCHAR(255), product VARCHAR(255), ecosystem VARCHAR(255), PRIMARY KEY (vendor, product));
INSERT INTO cpeEcosystemCache (vendor, product, ecosystem) VALUES ('apache', 'zookeeper', 'MULTIPLE');
INSERT INTO cpeEcosystemCache (vendor, product, ecosystem) VALUES ('tensorflow', 'tensorflow', 'MULTIPLE');
INSERT INTO cpeEcosystemCache (vendor, product, ecosystem) VALUES ('scikit-learn', 'scikit-learn', 'MULTIPLE');

CREATE INDEX idxCwe ON cweEntry(cveid);
CREATE INDEX idxVulnerability ON vulnerability(cve);
CREATE INDEX idxReference ON `reference`(cveid);
CREATE INDEX idxCpe ON cpeEntry(vendor, product);
CREATE INDEX idxSoftwareCve ON software(cveid);
CREATE INDEX idxSoftwareCpe ON software(cpeEntryId);

#on mysql we cannot index all columns due to key length issues
CREATE INDEX idxCpeEntry ON cpeEntry(part, vendor, product, version);
#, update_version, edition, lang, sw_edition, target_sw, target_hw, other);


CREATE TABLE properties (id varchar(50) PRIMARY KEY, `value` varchar(500));




INSERT INTO properties(id, value) VALUES ('version', '5.2');



CREATE TABLE `cluster_datasync_task` (
                                         `id` varchar(64) COLLATE utf8mb4_bin NOT NULL COMMENT '主键ID',
                                         `host` varchar(64) COLLATE utf8mb4_bin NOT NULL COMMENT '执行主机IP',
                                         `data_json` text COLLATE utf8mb4_bin COMMENT 'data json串',
                                         `task_type` tinyint(4) DEFAULT NULL COMMENT '1: storage/ 2:Repository',
                                         `status` tinyint(4) NOT NULL COMMENT '(0:为完成 1：已完成)',
                                         `url` varchar(128) COLLATE utf8mb4_bin DEFAULT NULL COMMENT '请求地址IP',
                                         `current_time_millis` bigint(20) DEFAULT NULL,
                                         PRIMARY KEY (`id`) USING BTREE,
                                         KEY `cluster_datasync_task_host_IDX` (`host`) USING BTREE,
                                         KEY `cluster_datasync_task_status_IDX` (`status`) USING BTREE,
                                         KEY `cluster_datasync_task_task_type_IDX` (`task_type`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin COMMENT='集群模式下的制品文件数据同步';


CREATE TABLE `folib_lock` (
                              `name` varchar(64) COLLATE utf8mb4_bin NOT NULL,
                              `lock_until` timestamp(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3) ON UPDATE CURRENT_TIMESTAMP(3),
                              `locked_at` timestamp(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3),
                              `locked_by` varchar(255) COLLATE utf8mb4_bin NOT NULL,
                              PRIMARY KEY (`name`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;


CREATE TABLE `folib_scanner` (
                                 `path` varchar(255) NOT NULL COMMENT '制品路径',
                                 `repository` varchar(255) NOT NULL COMMENT '仓库名称',
                                 `storage` varchar(255) NOT NULL COMMENT '存储空间',
                                 `artifact_path` varchar(255) DEFAULT NULL,
                                 `report` longtext COMMENT '报告',
                                 `file_type` varchar(10) DEFAULT NULL COMMENT '文件类型',
                                 `scan_status` varchar(10) DEFAULT NULL COMMENT '扫描状态',
                                 `on_scan` tinyint(1) DEFAULT NULL COMMENT '是否扫描',
                                 `scan_time` datetime DEFAULT NULL COMMENT '扫描时间',
                                 `level` varchar(32) DEFAULT NULL COMMENT '安全级别',
                                 `dependency_count` int(11) DEFAULT NULL COMMENT '依赖数量',
                                 `vulnerable_count` int(11) DEFAULT NULL COMMENT '容易被攻击的依赖数量',
                                 `vulnerabilites_count` int(11) DEFAULT NULL COMMENT '漏洞数量',
                                 `suppressed_count` int(11) DEFAULT NULL COMMENT '被封存的数量',
                                 `cve_checked_time` datetime DEFAULT NULL,
                                 `cve_update_time` datetime DEFAULT NULL,
                                 PRIMARY KEY (`path`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;


CREATE TABLE `scan_rules` (
                              `id` varchar(255) NOT NULL,
                              `repository` varchar(255) DEFAULT NULL COMMENT '仓库名称',
                              `storage` varchar(255) DEFAULT NULL COMMENT '存储空间',
                              `layout` varchar(255) DEFAULT NULL COMMENT '仓库类型',
                              `on_scan` tinyint(1) DEFAULT NULL COMMENT '是否扫描',
                              `scan_rule` varchar(255) DEFAULT NULL COMMENT '扫描规则',
                              PRIMARY KEY (`id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

SET FOREIGN_KEY_CHECKS = 1;
