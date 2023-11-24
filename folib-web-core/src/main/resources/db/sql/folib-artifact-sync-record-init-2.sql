alter table `artifact_sync_record`
    add column `request_host_name` varchar(255) default '' null comment '请求主机名称' after id;