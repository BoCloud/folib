package com.veadan.folib.enums;

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
public enum DockerTypeEnum {

    /**
     * push
     */
    PUSH("push"),
    /**
     * pull
     */
    PULL("pull"),
    ;

    /**
     * type
     */
    private String type;

}
