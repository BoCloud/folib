alter table artifact_sync_record
    add column request_host_name varchar(255) default '';
comment on column "artifact_sync_record"."request_host_name" is '请求主机名称';

alter table artifact_sync_record
    add column source_storage_id longvarchar;
comment on column "artifact_sync_record"."source_storage_id" is '源制品存储空间ID';

alter table artifact_sync_record
    add column source_repository_id longvarchar;
comment on column "artifact_sync_record"."source_repository_id" is '源制品仓库ID';

alter table artifact_sync_record
    add column sync_progress double;
comment on column "artifact_sync_record"."sync_progress" is '同步进度（只有被清除从数据采用持久化）';
