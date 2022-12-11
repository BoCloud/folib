package com.veadan.folib.controllers.cluster.dto;

import com.veadan.folib.cluster.SyncMetadataEnum;
import com.veadan.folib.cluster.SyncRepositoryEnum;
import com.veadan.folib.configuration.MutableMetadataConfiguration;
import com.veadan.folib.storage.repository.RepositoryDto;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotNull;

/**
 * @author leipenghui
 *
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class SyncMetadataDto {

    private MutableMetadataConfiguration mutableMetadataConfiguration;

    private SyncMetadataEnum syncMetadataEnum;
}
