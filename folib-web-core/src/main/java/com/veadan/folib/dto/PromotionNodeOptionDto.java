package com.veadan.folib.dto;

import com.veadan.folib.providers.io.RepositoryPath;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.File;
import java.io.InputStream;
import java.nio.file.Path;
import java.util.Map;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class PromotionNodeOptionDto {
    private String storageId;
    private String repositoryId;
    private Map<String, Map<String, RepositoryPath>> pathMap;
    private Map<String, Object> fileMetaDataMap;
    private boolean isRetry;
}
