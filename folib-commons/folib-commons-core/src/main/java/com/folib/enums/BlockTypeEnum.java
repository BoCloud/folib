package com.folib.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @author veadan
 * @date 2022/10/24
 **/
@AllArgsConstructor
@NoArgsConstructor
@Getter
public enum BlockTypeEnum {

    /**
     * 全量阻断
     */
    ALL(1),
    /**
     * 黑名单阻断
     */
    BLACK(2),
    /**
     * 包名阻断
     */
    PACKAGE_NAME(3),
    ;

    private Integer type;

}
