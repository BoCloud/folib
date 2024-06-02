DROP TABLE IF EXISTS artifact_sync_slave_record;

CREATE TABLE artifact_sync_slave_record (
	"id" BIGINT NOT NULL AUTO_INCREMENT COMMENT '主键',
	"source_path" VARCHAR(6000) DEFAULT '' NULL COMMENT '制品源路径',
	"target_path" VARCHAR(6000) DEFAULT '' NULL COMMENT '制品目标路径',
	"sync_no" VARCHAR(64) DEFAULT '' NULL COMMENT '制品同步编号',
	"failed_reason" LONGVARCHAR DEFAULT '' NULL COMMENT '失败的原因',
	"sync_model" INT DEFAULT 1 NULL COMMENT '同步模式（1：推；2：拉）',
	"update_by" VARCHAR(255) DEFAULT '' NULL COMMENT '更新人',
	"update_time" DATETIME NULL COMMENT '更新时间',
	"create_time" DATETIME DEFAULT CURRENT_TIMESTAMP NULL COMMENT '创建时间',
	"create_by" VARCHAR(255) DEFAULT '' NULL COMMENT '创建人',
	"status" INT NULL COMMENT '同步状态（1：就绪；2：同步中；3：成功；4：失败）',
	PRIMARY KEY ("id")
);

COMMENT ON TABLE artifact_sync_slave_record is '制品同步从记录';

