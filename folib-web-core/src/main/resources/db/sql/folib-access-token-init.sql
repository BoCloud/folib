DROP TABLE IF EXISTS `access_token`;

CREATE TABLE access_token
(
    id          int auto_increment NOT NULL PRIMARY KEY,
    token_id    varchar(50)           NULL COMMENT '令牌标识',
    description varchar(200)          NULL COMMENT '描述',
    operator    varchar(100)          NULL COMMENT '操作人',
    expire_time TIMESTAMP             NULL COMMENT '过期时间',
    username       varchar(50)         NULL COMMENT '用户',
    create_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
    UNIQUE INDEX unique_token(token_id)
)
    ENGINE = InnoDB
    CHARSET = utf8mb4
    COLLATE = utf8mb4_bin COMMENT ='访问令牌表';
