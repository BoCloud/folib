alter table artifact_sync_slave_record
    add file_size BIGINT UNSIGNED  DEFAULT 0 comment '文件大小(单位是字节)';