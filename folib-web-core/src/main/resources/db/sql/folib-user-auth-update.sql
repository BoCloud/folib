CREATE INDEX idx_user_type ON folib_user (user_type);
 
ALTER TABLE role_resource_ref MODIFY COLUMN ref_type int NULL COMMENT '关联类型[用户、用户组];[1-用户id、2-用户组id]';

CREATE INDEX idx_ref_type_entity_id ON role_resource_ref (ref_type,entity_id);