DROP TABLE IF EXISTS `migrate_info`;

CREATE TABLE migrate_info (
     id BIGINT AUTO_INCREMENT PRIMARY KEY COMMENT 'id',
     storage_id VARCHAR(255) COMMENT '存储空间',
     repository_id VARCHAR(100) COMMENT '仓库',
     migrate_id VARCHAR(100) COMMENT '迁移id',
     layout VARCHAR(50) COMMENT '布局',
     migrate_type VARCHAR(255) COMMENT 'jfrog',
     sync_status INT COMMENT '迁移状态',
     total_artifact INT COMMENT '制品总数',
     success_mount INT COMMENT '迁移成功量',
     sync_property INT COMMENT '是否迁移元数据',
     sync_dir_path VARCHAR(255) COMMENT '同步文件路径',
     used_space VARCHAR(255) COMMENT '原制品大小',
    KEY `idx_migrate_id` (`migrate_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='迁移数据表';