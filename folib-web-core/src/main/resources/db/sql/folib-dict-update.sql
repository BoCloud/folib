ALTER TABLE `dict`
MODIFY COLUMN `dict_type` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT 'upload_process' COMMENT '字典类型' AFTER `create_time`,
MODIFY COLUMN `dict_key` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT '字典key' AFTER `dict_type`,
MODIFY COLUMN `dict_value` varchar(512) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NULL DEFAULT '' COMMENT '字典value' AFTER `dict_key`;