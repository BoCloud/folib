package com.folib.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * 状态类型枚举
 *
 * @author veadan
 **/
@AllArgsConstructor
@NoArgsConstructor
@Getter
public enum WebhookEventsStatusEnum {

    /**
     * INIT
     */
    INIT(1),
    /**
     * SUCCESS
     */
    SUCCESS(2),
    /**
     * FAILURE
     */
    FAILURE(3),
    ;

    /**
     * status
     */
    private int status;


}
