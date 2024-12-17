INSERT INTO audit_event (module_value, module_name, event_value,event_name,used) VALUES ('ARTIFACT_REPOSITORY', '制品仓库', 'DOWNLOAD_EXCEPTION','下载异常',1);

update  audit_event set event_value ='ADD_USER_GROUP' , event_name='新增用户组' where event_value='USER_GROUP';
update  audit_event set event_value ='ADD_PERMISSIONS' , event_name='新增权限' where event_value='USER_MANAGEMENT';

update  audit_event set event_value ='ADD_USER_GROUP' , event_name='新增用户组' where event_value='USER_GROUP';
update  audit_event set event_value ='ADD_PERMISSIONS' , event_name='新增权限' where event_value='USER_MANAGEMENT';

INSERT INTO audit_event (module_value, module_name, event_value,event_name,used) VALUES ('USER_MANAGEMENT', '用户管理', 'UPDATE_USER_GROUP','修改用户组',1);
INSERT INTO audit_event (module_value, module_name, event_value,event_name,used) VALUES ('USER_MANAGEMENT', '用户管理', 'DELETE_USER_GROUP','删除用户组',1);

INSERT INTO audit_event (module_value, module_name, event_value,event_name,used) VALUES ('USER_MANAGEMENT', '用户管理', 'UPDATE_PERMISSIONS','修改权限',1);
INSERT INTO audit_event (module_value, module_name, event_value,event_name,used) VALUES ('USER_MANAGEMENT', '用户管理', 'DELETE_PERMISSIONS','删除权限',1);