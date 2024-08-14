ALTER TABLE artifact_sync_slave_record ADD file_size BIGINT UNSIGNED  DEFAULT 0 COMMENT '文件大小(单位是字节)';

ALTER TABLE artifact_sync_record ADD retry_count INT DEFAULT 0 COMMENT '重试次数' AFTER `retry`;

ALTER TABLE artifact_sync_record ADD retry_time datetime NULL COMMENT '重试时间' AFTER `retry_count`;