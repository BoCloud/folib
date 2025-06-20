package com.veadan.folib.dto;

import com.veadan.folib.dto.configuration.RepositoryDto;
import com.veadan.folib.dto.configuration.StorageDto;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class DispatchRepoCheckDto {

    private RepositoryDto repositoryForm;

    private StorageDto storageForm;

}
