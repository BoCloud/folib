package com.veadan.folib.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @author leipenghui
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
    BLACK(0),
    ;

    private Integer type;

}
