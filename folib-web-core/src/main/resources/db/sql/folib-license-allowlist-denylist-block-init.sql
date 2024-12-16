DROP TABLE IF EXISTS allowlist_denylist_block;
create table allowlist_denylist_block
(
    id           bigint auto_increment primary key,
    identifier   varchar(255) not null comment '标识具体规则如漏洞ID',
    type         varchar(32)  not null comment '类型[WHITES:白名单,BLACKLIST：黑名单]',
    valid_from   datetime     null comment '有效期',
    category       varchar(32)  not null comment '类别[VULNERABILITY:漏洞,LICENSE:许可证]',
    tag          varchar(12) not null comment '标签[DEFAULT:标记为老数据适配，LATEST:标记新建的]',
    domain          varchar(32) not null comment '业务域[SYSTEM:系统，REPOSITORY:仓库]',
    correlation_id varchar(255) null comment '关联ID[仓库ID]',
    description  varchar(255) null comment '描述',
    created_by   varchar(32)  null comment '创建人',
    created_time datetime     null comment '创建时间',
    updated_by   varchar(32)  null comment '更新人',
    update_time  datetime     null comment '更新时间'
)
    comment '黑白名单阻断';
DROP INDEX IF EXISTS allowlist_denylist_block_type_identifier_category_index on allowlist_denylist_block;

create index allowlist_denylist_block_type_identifier_category_index
    on allowlist_denylist_block (type, identifier, category);