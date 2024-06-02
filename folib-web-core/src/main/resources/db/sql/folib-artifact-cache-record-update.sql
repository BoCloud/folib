ALTER TABLE artifact_cache_record ADD node_id varchar(255) DEFAULT '';
COMMENT ON COLUMN "artifact_cache_record"."node_id" is '节点ID';

CREATE UNIQUE INDEX un_idx_node_id_storage_id_repository_id_artifact_path_prefix ON artifact_cache_record (node_id, storage_id, repository_id, artifact_path_prefix);