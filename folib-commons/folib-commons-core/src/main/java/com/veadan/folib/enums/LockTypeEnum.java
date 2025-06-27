package com.veadan.folib.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.apache.commons.lang3.StringUtils;

/**
 * @author veadan
 **/
@AllArgsConstructor
@NoArgsConstructor
@Getter
public enum LockTypeEnum {

    /**
     * 本地锁
     */
    LOCAL("local"),
    /**
     * 分布式锁
     */
    DISTRIBUTION("distribution"),
    ;

    private String type;

    public static String queryType(String value) {
        if (StringUtils.isBlank(value)) {
            return LOCAL.getType();
        }
        String type = "";
        for (LockTypeEnum lockTypeEnum : LockTypeEnum.values()) {
            if (lockTypeEnum.getType().equalsIgnoreCase(value)) {
                type = lockTypeEnum.getType();
                break;
            }
        }
        if (StringUtils.isBlank(type)) {
            return LOCAL.getType();
        }
        return type;
    }
}
