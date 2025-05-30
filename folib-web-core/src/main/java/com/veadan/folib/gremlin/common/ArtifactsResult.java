package com.veadan.folib.gremlin.common;

import lombok.Data;
import lombok.experimental.Accessors;

@Data
@Accessors(chain = true)
public class ArtifactsResult {

    private String storageId;
    private String repositoryId;
    private String artifactName;
    private String artifactPath;
    private Long sizeInBytes;
    private String path;
    
}
