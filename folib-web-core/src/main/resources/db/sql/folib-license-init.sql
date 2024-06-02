DROP TABLE IF EXISTS license;
CREATE TABLE license (
  id bigint NOT NULL AUTO_INCREMENT COMMENT '主键ID',
  create_time datetime DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  license_id varchar(255)  DEFAULT '' COMMENT '许可证id',
  license_name varchar(255)  DEFAULT '' COMMENT '许可证名称',
  license_url longvarchar  COMMENT '许可证地址',
  is_custom_license tinyint DEFAULT '0' COMMENT '是否是自定义许可证 1是 0否',
  is_deprecated tinyint DEFAULT '0' COMMENT '是否已弃用 1是 0否',
  is_osi_approved tinyint DEFAULT '0' COMMENT '是否属于OSI-Approved授权协议 1是 0否',
  is_fsf_libre tinyint DEFAULT '0' COMMENT '是否为自由软件基金会 1是 0否',
  header longvarchar  COMMENT '许可证头信息',
  template longvarchar  COMMENT '许可证模板',
  content longvarchar  COMMENT '许可证原文内容',
  content_cn longvarchar  COMMENT '许可证中文内容',
  "comment" longvarchar  COMMENT '备注',
  PRIMARY KEY (id)
);

COMMENT ON TABLE license is '许可证';

CREATE INDEX idx_license_license_id ON license (license_id);
CREATE INDEX idx_license_license_name ON license (license_name);