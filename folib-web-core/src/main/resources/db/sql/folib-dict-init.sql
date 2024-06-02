DROP TABLE IF EXISTS dict;

CREATE TABLE dict (
  id bigint NOT NULL AUTO_INCREMENT COMMENT '主键ID',
  create_time datetime DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  dict_type varchar(20)  DEFAULT 'upload_process' COMMENT '字典类型',
  dict_key varchar(50)  NOT NULL COMMENT '字典key',
  dict_value varchar(255)  DEFAULT '' COMMENT '字典value',
  alias longvarchar  DEFAULT '' COMMENT '别名',
  "comment" varchar(255)  DEFAULT '' COMMENT '备注',
  PRIMARY KEY (id)
);

COMMENT ON TABLE dict is '字典';

CREATE INDEX idx_dict_dict_key ON dict (dict_key);
CREATE INDEX idx_dict_dict_type ON dict (dict_type);