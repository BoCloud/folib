package com.veadan.folib.controllers.unicom;

import com.veadan.folib.forms.configuration.StorageForm;
import com.veadan.folib.storage.repository.RepositoryDto;
import com.veadan.folib.validation.configuration.UniqueStorage;
import lombok.Data;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.Pattern;
import java.util.List;

/**
 * @author huayanjun
 * @since 2024-09-23 15:52
 */
@Data
public class UnicomStorageDTO {


    @NotEmpty(message = "An id must be specified.")
    @UniqueStorage(groups = StorageForm.NewStorage.class, message = "The storage id already exists.")
    @Pattern(regexp = "[a-zA-Z0-9\\-\\_\\.]+")
    private String id;

    private String projectName;

    private List<String> layouts;

    // local、s3
    private String storageProvider;
}
