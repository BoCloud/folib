package com.veadan.folib.domain.policy.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class SyncArtifatDTO {

    private long policyId;
    private  String storageId;
    private  String repositoryId;
    private  String artifactPath;
}
