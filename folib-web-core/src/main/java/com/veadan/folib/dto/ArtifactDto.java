package com.veadan.folib.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ArtifactDto {
    private String storageId;
    private String repostoryId;
    private String path;
}
