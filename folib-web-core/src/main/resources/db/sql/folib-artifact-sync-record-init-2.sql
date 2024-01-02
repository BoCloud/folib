alter table `artifact_sync_record`
    add column `request_host_name` varchar(255) default '' null comment '请求主机名称' after `id`;
alter table `artifact_sync_record`
    add column source_storage_id text null comment '源制品存储空间ID' after `request_host_name`;
alter table `artifact_sync_record`
    add column source_repository_id text null comment '源制品仓库ID' after `source_storage_id`;
alter table `artifact_sync_record`
    add column sync_progress double null comment '同步进度（只有被清除从数据采用持久化）' after `ops_type`;