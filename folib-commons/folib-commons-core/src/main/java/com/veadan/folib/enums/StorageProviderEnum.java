package com.veadan.folib.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @author veadan
 * @date 2024/01/02
 **/
@AllArgsConstructor
@NoArgsConstructor
@Getter
public enum StorageProviderEnum {

    /**
     * s3
     */
    S3("s3"),
    /**
     * NFS
     */
    LOCAL("local"),
    ;

    private String type;

}
