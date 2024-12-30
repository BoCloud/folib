package com.veadan.folib.domain;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;

/**
 * @author leipenghui
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ExceedsSizeStorage {

    private String storageId;

    //存储空间的总大小
    private BigDecimal storageSize;

    //已使用的存储空间大小
    private BigDecimal useStorageSize;

    //已使用存储空间的比例
    private BigDecimal useStorageProportion;

}
