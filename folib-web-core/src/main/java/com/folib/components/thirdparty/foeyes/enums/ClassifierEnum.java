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
public enum ClassifierEnum {

    /**
     * Library
     */
    LIBRARY("LIBRARY"),

    /**
     * File
     */
    FILE("FILE"),
    ;

    private String type;
}
