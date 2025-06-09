package com.veadan.folib.gremlin.common;

import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

@Data
@Accessors(chain = true)
public class ArtifactsResult {

    private String storageId;
    private String repositoryId;
    private String artifactName;
    private String artifactPath;
    private BigDecimal sizeInBytes;
    private String path;
    
}
