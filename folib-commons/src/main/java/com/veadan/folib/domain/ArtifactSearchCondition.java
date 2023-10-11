package com.veadan.folib.domain;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * @author leipenghui
 * @date 2023/10/11
 **/
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class ArtifactSearchCondition {

    /**
     * 存储空间
     */
    private String storageId;
    /**
     * 仓库
     */
    private String repositoryId;
    /**
     * 路径前缀
     */
    private String path;
    /**
     * 路径条件
     */
    private List<ArtifactNameCondition> artifactNameConditions;

    /**
     * 元数据条件
     */
    private List<ArtifactMetadataCondition> artifactMetadataConditions;

}
