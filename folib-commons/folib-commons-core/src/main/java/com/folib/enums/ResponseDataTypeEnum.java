package com.folib.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * 条件类型枚举
 *
 * @author veadan
 **/
@AllArgsConstructor
@NoArgsConstructor
@Getter
public enum ResponseDataTypeEnum {

    /**
     * 字符串
     */
    STRING("string"),
    /**
     * json
     */
    JSON("json"),
    ;

    /**
     * type
     */
    private String type;

}
