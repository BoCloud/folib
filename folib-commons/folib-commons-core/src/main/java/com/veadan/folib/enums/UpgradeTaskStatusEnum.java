package com.veadan.folib.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @author veadan
 **/
@AllArgsConstructor
@NoArgsConstructor
@Getter
public enum UpgradeTaskStatusEnum {

    /**
     * 未执行
     */
    UN_EXECUTED("unexecuted"),
    /**
     * 执行中
     */
    EXECUTING("executing"),
    /**
     * 执行成功
     */
    EXECUTED_SUCCESS("executed_success"),
    /**
     * 执行失败
     */
    EXECUTED_FAIL("executed_fail"),
    ;

    private String status;

}
