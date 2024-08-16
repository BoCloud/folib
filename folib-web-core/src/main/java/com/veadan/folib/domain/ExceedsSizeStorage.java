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

    private BigDecimal storageSize;
    
    private BigDecimal useStorageSize;

    private BigDecimal useStorageProportion;

}
