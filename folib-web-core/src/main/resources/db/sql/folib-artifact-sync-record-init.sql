DROP TABLE IF EXISTS `artifact_sync_record`;

CREATE TABLE `artifact_sync_record`
(
    `id`            bigint NOT NULL AUTO_INCREMENT COMMENT '主键ID',
    `source_path`   longtext CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci COMMENT '源制品路径',
    `target_path`   longtext CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci COMMENT '目标制品路径',
    `sync_no`       varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci DEFAULT NULL COMMENT '制品同步编号',
    `ops_type`      int                                                          DEFAULT NULL COMMENT '制品操作（1：制品晋级；2：制品分发）',
    `sync_model`    int                                                          DEFAULT NULL COMMENT '同步模式（1：推；2：拉）',
    `status`        int                                                          DEFAULT NULL COMMENT '同步状态（1：就绪；2：同步中；3：成功；4：失败）',
    `failed_reason` longtext COMMENT '失败的原因',
    `retry`         int                                                          DEFAULT NULL COMMENT '是否重试（0:不重试，1:重试）',
    `created_by`    varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci DEFAULT NULL COMMENT '创建人',
    `created_time`  datetime                                                     DEFAULT NULL COMMENT '创建时间',
    `updated_by`    varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci DEFAULT NULL COMMENT '更新人',
    `updated_time`  datetime                                                     DEFAULT NULL COMMENT '更新时间',
    PRIMARY KEY (`id`)
) ENGINE = InnoDB
  AUTO_INCREMENT = 1
  DEFAULT CHARSET = utf8mb4
  COLLATE = utf8mb4_general_ci COMMENT ='制品同步记录'
