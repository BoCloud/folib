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
public enum RepositoryScopeEnum {

    /**
     * 存储空间内
     */
    STORAGE(1),
    /**
     * 公开
     */
    OPEN(2),
    ;

    private Integer type;

}
