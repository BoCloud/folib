package com.veadan.folib.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @author veadan
 * @date 2023/12/28
 **/
@AllArgsConstructor
@NoArgsConstructor
@Getter
public enum FileUnitTypeEnum {

    /**
     * BYTES
     */
    BYTES("BYTES"),
    /**
     * KB
     */
    KB("KB"),
    /**
     * MB
     */
    MB("MB"),
    /**
     * GB
     */
    GB("GB"),
    /**
     * TB
     */
    TB("TB"),
    /**
     * PB
     */
    PB("PB"),
    ;

    private String unit;
}
