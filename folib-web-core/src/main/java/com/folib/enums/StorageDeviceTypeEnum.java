package com.folib.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * 目录数据类型枚举
 *
 * @author veadan
 **/
@AllArgsConstructor
@NoArgsConstructor
@Getter
public enum StorageDeviceTypeEnum {

    /**
     * NAS
     */
    NAS("NAS"),

    /**
     * 'S3
     */
    S3("S3"),
    ;

    /**
     * type
     */
    private String type;

}
