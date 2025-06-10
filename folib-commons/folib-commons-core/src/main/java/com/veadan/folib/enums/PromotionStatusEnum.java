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
public enum PromotionStatusEnum {

    /**
     * 等待晋级
     */
    WAIT("wait"),
    /**
     * 晋级成功
     */
    SUCCESS("success"),
    /**
     * 晋级失败
     */
    FAIL("fail"),
    /**
     * 晋级阻断
     */
    BLOCK("block"),
    ;

    private String status;

}
