package com.veadan.folib.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * 索引类型枚举
 *
 * @author veadan
 **/
@AllArgsConstructor
@NoArgsConstructor
@Getter
public enum PypiIndexTypeEnum {

    /**
     * reindex
     */
    REINDEX("reindex"),
    /**
     * add
     */
    ADD("add"),
    /**
     * delete
     */
    DELETE("delete"),
    ;

    /**
     * type
     */
    private String type;
}
