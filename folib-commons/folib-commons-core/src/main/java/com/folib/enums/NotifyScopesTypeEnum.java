package com.folib.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @author veadan
 * @date 2022/10/18
 **/
@AllArgsConstructor
@NoArgsConstructor
@Getter
public enum NotifyScopesTypeEnum {

    /**
     * admin
     */
    ADMIN("admin"),
    /**
     * storageAdmin
     */
    STORAGE_ADMIN("storageAdmin"),
    ;

    private String scope;

}
