package com.veadan.folib.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * 条件类型枚举
 *
 * @author leipenghui
 **/
@AllArgsConstructor
@NoArgsConstructor
@Getter
public enum ConditionTypeEnum {

    /**
     * 等于
     */
    EQ("eq"),
    /**
     * 范围
     */
    RANGE("range"),
    ;

    /**
     * condition
     */
    private String condition;

}
