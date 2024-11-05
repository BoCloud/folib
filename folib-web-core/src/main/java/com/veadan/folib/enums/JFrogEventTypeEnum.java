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
public enum JFrogEventTypeEnum {

    /**
     * deployed
     */
    DEPLOYED("deployed", true),
    /**
     * copied
     */
    COPIED("copied", false),
    /**
     * deleted
     */
    DELETED("deleted", false),
    /**
     * moved
     */
    MOVED("moved", false),
    /**
     * pushed
     */
    PUSHED("pushed", true),
    ;

    /**
     * type
     */
    private String type;

    /**
     * handle
     */
    private boolean handle;

    public static boolean needHandle(String type) {
        boolean result = false;
        for (JFrogEventTypeEnum jFrogEventTypeEnum : JFrogEventTypeEnum.values()) {
            if (jFrogEventTypeEnum.type.equals(type)) {
                result = Boolean.TRUE.equals(jFrogEventTypeEnum.handle);
                break;
            }
        }
        return result;
    }

}
