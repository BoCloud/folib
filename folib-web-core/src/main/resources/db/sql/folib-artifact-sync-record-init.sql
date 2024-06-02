DROP TABLE IF EXISTS artifact_sync_record;

CREATE TABLE artifact_sync_record (
  id bigint NOT NULL AUTO_INCREMENT COMMENT '主键ID',
  source_path varchar(6000) COMMENT '源制品路径',
  target_path varchar(6000) COMMENT '目标制品路径',
  sync_no varchar(64) DEFAULT '' COMMENT '制品同步编号',
  ops_type int DEFAULT 1 COMMENT '制品操作（1：制品晋级；2：制品分发）',
  sync_model int DEFAULT 2 COMMENT '同步模式（1：推；2：拉）',
  status int DEFAULT 1 COMMENT '同步状态（1：就绪；2：同步中；3：成功；4：失败）',
  failed_reason longvarchar COMMENT '失败的原因',
  retry int DEFAULT 0 COMMENT '是否重试（0:不重试，1:重试）',
  create_by varchar(255) DEFAULT '' COMMENT '创建人',
  create_time datetime DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  update_by varchar(255) DEFAULT '' COMMENT '更新人',
  update_time datetime DEFAULT NULL COMMENT '更新时间',
  PRIMARY KEY (id)
);

COMMENT ON TABLE artifact_sync_record is '制品同步记录';

CREATE UNIQUE INDEX uk_artifact_sync_record_sync_no ON artifact_sync_record (sync_no);
CREATE INDEX idx_artifact_sync_record_ops_type ON artifact_sync_record (ops_type);
CREATE INDEX idx_artifact_sync_record_status ON artifact_sync_record (status);