DROP TABLE IF EXISTS federal_promotion_policy;

create table federal_promotion_policy
(
    policy_id    BIGINT AUTO_INCREMENT COMMENT 'id',
    name         varchar(255) not null comment '联邦晋级策略名',
    is_enabled   VARCHAR(1) DEFAULT 0 COMMENT '是否开启策略',
    tag          varchar(12) null comment '标签[default:标记为老数据适配，latest:标记新建的]',
    created_time datetime null comment '创建时间',
    update_time  DATETIME   DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
    created_by   varchar(32) null comment '创建人',
    updated_by   varchar(32) null comment '更新人',
    PRIMARY KEY (policy_id),
    KEY          idx_is_enabled (is_enabled),
    KEY          idx_created_time (created_time),
    KEY          idx_update_time (update_time)
) comment '联邦晋级策略表';

create index federal_promotion_policy_name_tag_index
    on federal_promotion_policy (name, tag);

DROP TABLE IF EXISTS promotion_rule;
CREATE TABLE promotion_rule
(
    rule_id         BIGINT AUTO_INCREMENT COMMENT 'id',
    policy_id       LONG        NOT NULL COMMENT '策略ID',
    rule_type       VARCHAR(32) NOT NULL COMMENT '规则类型:[path, metadata]',
    attribute_key   VARCHAR(255) COMMENT '属性key',
    attribute_value VARCHAR(255) COMMENT '属性值]',
    update_time     DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
    created_time    DATETIME COMMENT '创建时间',
    PRIMARY KEY (rule_id),
    KEY             idx_created_time (created_time),
    KEY             idx_update_time (update_time)
) COMMENT = '联邦晋级规则';

DROP TABLE IF EXISTS federal_repository;
CREATE TABLE federal_repository
(
    id            BIGINT AUTO_INCREMENT COMMENT 'id',
    policy_id     LONG         NOT NULL COMMENT '策略ID',
    type          VARCHAR(32)  NOT NULL COMMENT '联邦库类型:[source，target]',
    storage_id    VARCHAR(255) COMMENT '存储空间ID',
    repository_id VARCHAR(255) NOT NULL COMMENT '仓库ID',
    node_name     VARCHAR(255)  COMMENT '节点名称：目标库才有的属性',
    node_type     VARCHAR(32) COMMENT '节点类型[inner:内部节点,external:外部节点]',
    created_time  DATETIME COMMENT '创建时间',
    update_time   DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '更新时间',
    PRIMARY KEY (id),
    KEY           idx_created_time (created_time),
    KEY           idx_update_time (update_time)
) COMMENT = '联邦仓库表';