package com.veadan.folib.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * 制品仓库类型枚举
 *
 * @author veadan
 **/
@AllArgsConstructor
@NoArgsConstructor
@Getter
public enum ArtifactoryRepositoryTypeEnum {

    /**
     * Folib
     */
    INNER("inner"),
    /**
     * JFrog
     */
    JFROG("JFrog"),
    ;

    /**
     * 类型
     */

    private String type;
}
