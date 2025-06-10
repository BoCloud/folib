package com.veadan.folib.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @author leipenghui
 **/
@AllArgsConstructor
@NoArgsConstructor
@Getter
public enum UnionRepositorySyncTypeEnum {

    /**
     * 制品路径
     */
    ARTIFACT_PATH(1),
    /**
     * 元数据
     */
    METADATA(2),
    ;

    private Integer type;

}
