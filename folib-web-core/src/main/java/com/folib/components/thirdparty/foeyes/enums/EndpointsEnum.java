package com.folib.components.thirdparty.foeyes.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @author veadan
 * @date 2024/4/22
 **/
@AllArgsConstructor
@NoArgsConstructor
@Getter
public enum EndpointsEnum {

    /**
     * 登录
     */
    USER_LOGIN("USER_LOGIN", "/api/v1/user/login"),

    /**
     * 创建项目
     */
    CREATE_PROJECT("CREATE_PROJECT", "/api/v1/project"),

    /**
     * 依据项目id查询项目
     */
    QUERY_PROJECT("QUERY_PROJECT", "/api/v1/project/%s"),

    /**
     * 按其名称和版本返回特定项目
     */
    LOOKUP_PROJECT("LOOKUP_PROJECT", "/api/v1/project/lookup"),

    /**
     * 上传bom
     */
    BOM_UPLOAD("BOM_UPLOAD", "/api/v1/bom"),

    ;

    /**
     * name
     */
    private String name;

    /**
     * path
     */
    private String path;
}
