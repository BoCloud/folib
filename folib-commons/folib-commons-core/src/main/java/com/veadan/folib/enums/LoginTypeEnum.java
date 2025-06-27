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
public enum LoginTypeEnum {

    /**
     * 本地认证
     */
    STANDARD("STANDARD"),
    /**
     * LDAP
     */
    LDAP("LDAP"),
    ;

    private String type;

    public static String queryType(String value) {
        if (StringUtils.isBlank(value)) {
            return STANDARD.getType();
        }
        String type = "";
        for (LoginTypeEnum loginTypeEnum : LoginTypeEnum.values()) {
            if (loginTypeEnum.getType().equalsIgnoreCase(value)) {
                type = loginTypeEnum.getType();
                break;
            }
        }
        if (StringUtils.isBlank(type)) {
            return STANDARD.getType();
        }
        return type;
    }
}
