ALTER TABLE artifact_sync_slave_record ENGINE = InnoDB;
ALTER TABLE artifact_sync_slave_record MODIFY COLUMN source_path longtext CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL COMMENT '制品源路径';
ALTER TABLE artifact_sync_slave_record MODIFY COLUMN target_path longtext CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL COMMENT '制品目标路径';