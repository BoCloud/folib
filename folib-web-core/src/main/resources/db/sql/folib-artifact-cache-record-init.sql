DROP TABLE IF EXISTS artifact_cache_record;

CREATE TABLE artifact_cache_record (
  id bigint NOT NULL COMMENT '主键ID',
  storage_id varchar(255) DEFAULT '' COMMENT '存储空间名称',
  repository_id varchar(255) DEFAULT '' COMMENT '仓库名称',
  artifact_path longvarchar COMMENT '制品路径',
  artifact_path_prefix varchar(768) DEFAULT '' COMMENT '制品路径前缀',
  size bigint DEFAULT '0' COMMENT '制品大小',
  md5 varchar(255) DEFAULT '' COMMENT 'md5',
  sha1 varchar(255) DEFAULT '' COMMENT 'sha1',
  sha256 varchar(255) DEFAULT '' COMMENT 'sha256',
  download_count bigint DEFAULT '0' COMMENT '下载次数',
  latest_download_time datetime DEFAULT NULL COMMENT '最后下载时间',
  cache_directory_path varchar(1000) DEFAULT '' COMMENT '缓存目录',
  cache_path longvarchar COMMENT '缓存路径',
  create_time datetime DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  update_time datetime DEFAULT NULL COMMENT '更新时间',
  PRIMARY KEY (id)
);

COMMENT ON TABLE artifact_cache_record is '制品缓存记录';

CREATE INDEX idx_artifact_cache_record_storage_id ON artifact_cache_record (storage_id);
CREATE INDEX idx_artifact_cache_record_repository_id ON artifact_cache_record (repository_id);
CREATE INDEX idx_artifact_cache_record_artifact_path_prefix ON artifact_cache_record (artifact_path_prefix);