DROP TABLE IF EXISTS allowlist_denylist_block;
create table allowlist_denylist_block
(
    id           bigint auto_increment primary key,
    identifier   varchar(255) not null comment '标识具体规则如漏洞ID',
    type         varchar(32)  not null comment '类型[whites:白名单,blacklist：黑名单]',
    valid_from   datetime     null comment '有效期',
    category       varchar(32)  not null comment '类别[VULN:漏洞,LICENSE:许可证]',
    tag          varchar(12) null comment '标签[default:标记为老数据适配，latest:标记新建的]',
    created_by   varchar(32)  null comment '创建人',
    created_time datetime     null comment '创建时间',
    updated_by   varchar(32)  null comment '更新人',
    update_time  datetime     null comment '更新时间'
)
    comment '黑白名单阻断';
drop index allowlist_denylist_block_type_identifier_category_index on allowlist_denylist_block;

create index allowlist_denylist_block_type_identifier_category_index
    on allowlist_denylist_block (type, identifier, category);

