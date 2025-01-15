alter table migrate_info add post_layout varchar(100) null comment '修改后的布局';
alter table migrate_info add index_finish tinyint(1) default 0 comment '索引是否完成';