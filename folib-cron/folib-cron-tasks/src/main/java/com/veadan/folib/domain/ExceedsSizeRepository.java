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
public class ExceedsSizeRepository {

    private String storageId;

    private String repositoryId;

    private String layout;

    private BigDecimal repositoryMaxSize;

    private BigDecimal useRepositorySize;

    private BigDecimal useRepositoryProportion;

}
