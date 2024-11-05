DROP TABLE IF EXISTS git_lfs_locks;
CREATE TABLE git_lfs_locks(
                              `id` VARCHAR(255) NOT NULL  COMMENT 'id' ,
                              `storage_id` VARCHAR(255) NOT NULL  COMMENT '存储ID' ,
                              `repository_id` VARCHAR(255) NOT NULL   COMMENT '仓库ID' ,
                              `path` VARCHAR(255) NOT NULL   COMMENT '锁定文件的路径' ,
                              `locked_at` BIGINT NOT NULL  COMMENT '创建锁的时间戳' ,
                              `owner` VARCHAR(255) NOT NULL  COMMENT '所属人' ,
                              `ref` VARCHAR(255) NOT NULL   COMMENT '描述锁所属的服务器引用' ,
                              PRIMARY KEY (id)
)  ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='git lfs 锁表';