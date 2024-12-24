SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;
DROP TABLE IF EXISTS `allowlist_denylist_block`;
CREATE TABLE `allowlist_denylist_block` (
                                            `id` bigint(20) NOT NULL AUTO_INCREMENT,
                                            `identifier` varchar(255) NOT NULL COMMENT '标识具体规则如漏洞ID',
                                            `type` varchar(32) NOT NULL COMMENT '类型[WHITES:白名单,BLACKLIST：黑名单]',
                                            `valid_from` datetime DEFAULT NULL COMMENT '有效期',
                                            `category` varchar(32) NOT NULL COMMENT '类别[VULNERABILITY:漏洞,LICENSE:许可证]',
                                            `tag` varchar(12) NOT NULL COMMENT '标签[DEFAULT:标记为老数据适配，LATEST:标记新建的]',
                                            `domain` varchar(32) NOT NULL COMMENT '业务域[SYSTEM:系统，REPOSITORY:仓库]',
                                            `correlation_id` varchar(255) DEFAULT NULL COMMENT '关联ID[仓库ID]',
                                            `description` varchar(255) DEFAULT NULL COMMENT '描述',
                                            `created_by` varchar(32) DEFAULT NULL COMMENT '创建人',
                                            `created_time` datetime DEFAULT NULL COMMENT '创建时间',
                                            `updated_by` varchar(32) DEFAULT NULL COMMENT '更新人',
                                            `update_time` datetime DEFAULT NULL COMMENT '更新时间',
                                            PRIMARY KEY (`id`),
                                            KEY `allowlist_denylist_block_type_identifier_category_index` (`type`,`identifier`,`category`)
) ENGINE=InnoDB AUTO_INCREMENT=12 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci COMMENT='黑白名单阻断';

SET FOREIGN_KEY_CHECKS = 1;