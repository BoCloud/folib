DROP TABLE IF EXISTS external_node;

CREATE TABLE external_node (
  id bigint NOT NULL AUTO_INCREMENT COMMENT '主键ID',
  create_time datetime DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  node_name varchar(255)  DEFAULT '' COMMENT '节点名称',
  type varchar(255)  DEFAULT '' COMMENT '制品库类型',
  address varchar(512)  DEFAULT '' COMMENT '节点地址',
  username varchar(255)  DEFAULT '' COMMENT '用户名',
  password varchar(512)  DEFAULT '' COMMENT '密码',
  "comment" varchar(255)  DEFAULT '' COMMENT '备注',
  PRIMARY KEY (id)
);

COMMENT ON TABLE external_node is '外部节点';

CREATE UNIQUE INDEX uk_external_node_node_name ON external_node (node_name);
CREATE INDEX idx_external_node_type ON external_node (type);
