package com.folib.dto;

import com.folib.forms.configuration.RepositoryForm;
import com.folib.forms.configuration.StorageForm;
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
