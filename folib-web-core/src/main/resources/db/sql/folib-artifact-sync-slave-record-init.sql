DROP TABLE IF EXISTS `artifact_sync_slave_record`;

create table artifact_sync_slave_record
(
    id            bigint auto_increment comment '主键'
        primary key,
    source_path   longtext                               null comment '制品源路径',
    target_path   longtext                               null comment '制品目标路径',
    sync_no       varchar(64)                            null comment '制品同步编号',
    failed_reason longtext                               null comment '失败的原因',
    sync_model    int                                    null comment '同步模式（1：推；2：拉）',
    update_by     varchar(255) default ''                null comment '更新人',
    update_time   datetime                               null comment '更新时间',
    create_time   datetime     default CURRENT_TIMESTAMP null comment '创建时间',
    create_by     varchar(255) default ''                null comment '创建人',
    status        int                                    null comment '同步状态（1：就绪；2：同步中；3：成功；4：失败）'
)
    comment '制品同步从记录';

