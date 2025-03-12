ALTER TABLE `audit_log_record` MODIFY `module` varchar(255) comment '事件模块';

ALTER TABLE `audit_log_record` MODIFY `module_name` varchar(255) comment '模块名称';

ALTER TABLE `audit_log_record` MODIFY `name` varchar(255) comment '事件名称';

ALTER TABLE `audit_log_record` MODIFY `event_name` varchar(255) comment '事件中文名';

ALTER TABLE `audit_log_record` MODIFY `username` varchar(255) comment '操作人';

ALTER TABLE `audit_log_record` MODIFY `target` varchar(255) comment '事件对象';