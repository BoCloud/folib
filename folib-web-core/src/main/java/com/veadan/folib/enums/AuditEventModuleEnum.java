package com.veadan.folib.enums;

import lombok.Getter;

/**
 * @author veadan
 * @since 2024-08-12 16:31
 */
@Getter
public enum AuditEventModuleEnum {

    ARTIFACT_REPOSITORY("制品仓库"),
    USER_MANAGEMENT("用户管理"),
//    AUTHENTICATION_MANAGEMENT("认证管理"),
    PROXY(" 网络代理"),
    SYSTEM_SETTING("系统设置"),
    ADVANCE_SETTING("高级运维");


    private String name;

    AuditEventModuleEnum(String name) {
        this.name = name;
    }


}
