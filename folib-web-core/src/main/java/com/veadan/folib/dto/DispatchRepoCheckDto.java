package com.veadan.folib.dto;

import com.veadan.folib.forms.configuration.RepositoryForm;
import com.veadan.folib.forms.configuration.StorageForm;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class DispatchRepoCheckDto {

    private RepositoryForm repositoryForm;

    private StorageForm storageForm;

}
