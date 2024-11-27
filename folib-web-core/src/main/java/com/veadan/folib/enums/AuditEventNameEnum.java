package com.veadan.folib.enums;

import lombok.Getter;

import static com.veadan.folib.enums.AuditEventModuleEnum.ADVANCE_SETTING;
import static com.veadan.folib.enums.AuditEventModuleEnum.ARTIFACT_REPOSITORY;
import static com.veadan.folib.enums.AuditEventModuleEnum.SYSTEM_SETTING;
import static com.veadan.folib.enums.AuditEventModuleEnum.USER_MANAGEMENT;

/**
 * @author huayanjun
 * @since 2024-08-12 16:35
 * 新建仓库
 * 删除仓库
 * 仓库权限
 * 仓库定时策略
 * 联邦仓库
 * 制品扫描
 * 制品删除
 * 制品目录删除
 * 制品更新
 * 元数据更新
 */
@Getter
public enum AuditEventNameEnum {

    ADD_STORAGE(ARTIFACT_REPOSITORY, "新建存储空间"),
    DELETE_STORAGE(ARTIFACT_REPOSITORY, "删除存储空间"),
    ADD_REPOSITORY(ARTIFACT_REPOSITORY, "新建仓库"),
    DELETE_REPOSITORY(ARTIFACT_REPOSITORY, "删除仓库"),
    PERMIT_REPOSITORY(ARTIFACT_REPOSITORY, "仓库权限"),
    CRON_REPOSITORY(ARTIFACT_REPOSITORY, "仓库定时策略"),
    UNION_REPOSITORY(ARTIFACT_REPOSITORY, "联邦仓库"),
    SCAN_ARTIfFACT(ARTIFACT_REPOSITORY, "制品扫描"),
    DELETE_ARTIfFACT(ARTIFACT_REPOSITORY, "删除制品"),
    UPLOAD_ARTIfFACT(ARTIFACT_REPOSITORY, "制品上传"),
//    UPDATE_META(SYSTEM_SETTING, "更新元数据"),


    CREATE_USER(USER_MANAGEMENT, "用户管理"),
    DELETE_USER(USER_MANAGEMENT, "删除用户"),
    USER_GROUP(USER_MANAGEMENT, "用户组"),
    USER_PERMISSION(USER_MANAGEMENT, "用户权限"),

    WEB_LOGIN(USER_MANAGEMENT, "web登录"),
    SSO_LOGIN(USER_MANAGEMENT, "sso登录"),

    BASE_SETTING(SYSTEM_SETTING, "基础信息配置"),
    SAFE_STRATEGY(SYSTEM_SETTING, "安全策略"),
    LDAP_SETTING(SYSTEM_SETTING, "LDAP配置"),
    SSO_SETTING(SYSTEM_SETTING, "SSO配置"),
    META_SETTING(SYSTEM_SETTING, "元数据配置"),
    UPDATE_META(SYSTEM_SETTING, "更新元数据"),
    NODE_DISPATCH(SYSTEM_SETTING, "节点分发配置"),
    WEB_HOOK(SYSTEM_SETTING, "webhook"),

    BUILD_GRAPH_INDEX(ADVANCE_SETTING, "构建数据"),
    MAVEN_INDEXER(ADVANCE_SETTING, "Maven数据迁移"),
    BUG_UPDATE(ADVANCE_SETTING, "漏洞更新"),
    BACKUP_STRATEGY(ADVANCE_SETTING, "备份策略"),
    CACHE_STRATEGY(ADVANCE_SETTING, "缓存策略"),
    REINDEX(ADVANCE_SETTING, "重建索引"),
    REGISTER_INDEX(ADVANCE_SETTING, "注册索引"),
    DELETE_INSTANCE(ADVANCE_SETTING, "删除实例");


    private AuditEventModuleEnum module;
    private String name;

    AuditEventNameEnum(AuditEventModuleEnum module, String name) {
        this.module = module;
        this.name = name;
    }
}
