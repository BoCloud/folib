package com.folib.enums;

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
public enum PubIndexTypeEnum {

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
