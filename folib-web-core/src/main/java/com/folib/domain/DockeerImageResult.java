package com.folib.domain;


import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

@Data
@Accessors(chain = true)
public class DockeerImageResult {

    private String storageId;
    private String repositoryId;
    private String artifactName;
    private String tag;
    private String path;
    private BigDecimal size;

}
