package com.veadan.folib.domain;


import lombok.Data;
import lombok.experimental.Accessors;

@Data
@Accessors(chain = true)
public class DockeerImageResult {

    private String storageId;
    private String repositoryId;
    private String artifactName;
    private String tag;
    private String path;
    private long size;

}
