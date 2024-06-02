DROP TABLE IF EXISTS webhook_log;

CREATE TABLE webhook_log (
  id bigint NOT NULL AUTO_INCREMENT COMMENT '主键ID',
  create_time datetime DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  event_type varchar(255)  DEFAULT '' COMMENT '事件类型',
  storage_id varchar(255)  DEFAULT '' COMMENT '存储空间名称',
  repository_id varchar(255)  DEFAULT '' COMMENT '仓库名称',
  artifact_path varchar(255)  DEFAULT '' COMMENT '制品路径',
  url varchar(255)  DEFAULT '' COMMENT '请求url',
  access_token varchar(255)  DEFAULT '' COMMENT '访问令牌',
  method varchar(255)  DEFAULT '' COMMENT '请求方式',
  completion_time decimal(16,2) DEFAULT '0.00' COMMENT '完成时间（秒）',
  request_headers longvarchar  COMMENT '请求头',
  request longvarchar  COMMENT '请求报文',
  response_status varchar(255)  DEFAULT '200' COMMENT '响应状态码',
  response_headers longvarchar  COMMENT '响应头',
  response longvarchar  COMMENT '响应报文',
  remark varchar(255)  DEFAULT '' COMMENT '备注',
  PRIMARY KEY (id)
);

COMMENT ON TABLE webhook_log is 'webhook';

CREATE INDEX idx_webhook_log_storage_id ON webhook_log (storage_id);
CREATE INDEX idx_webhook_log_repository_id ON webhook_log (repository_id);
CREATE INDEX idx_webhook_log_artifact_path ON webhook_log (artifact_path);