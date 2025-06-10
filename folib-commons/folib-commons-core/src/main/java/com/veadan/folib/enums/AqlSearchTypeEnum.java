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
public enum AqlSearchTypeEnum {

    /**
     * 目录
     */
    FOLDER("folder"),
    /**
     * 文件
     */
    FILE("file"),
    ;

    private String type;
}
