package com.veadan.folib.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class PromotionArtifactDto {
    private String srcStorageId;
    private String srcRepositoryId;
    private String targetStorageId;
    private String targetRepositoryId;
    private String path;
    private String uploadHost;

}
