package com.veadan.folib.controllers.cluster.dto;

import com.veadan.folib.cluster.SyncUnionRepositoryEnum;
import com.veadan.folib.dto.configuration.UnionRepositoryConfigurationDto;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author leipenghui
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class SyncUnionRepositoryDto {

    private String storageId;

    private String repositoryId;

    private UnionRepositoryConfigurationDto unionRepositoryConfigurationForm;

    private SyncUnionRepositoryEnum syncUnionRepositoryEnum;
}
