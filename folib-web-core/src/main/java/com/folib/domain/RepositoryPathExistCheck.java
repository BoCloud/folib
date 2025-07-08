package com.folib.domain;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author veadan
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class RepositoryPathExistCheck {

    /**
     * 存储空间
     */
    private String storageId;

    /**
     * 仓库
     */
    private String repositoryId;

    /**
     * 制品路径
     */
    private String artifactPath;

    /**
     * checksum类型
     */
    private String digestAlgorithm;

    /**
     * checksum值
     */
    private String digest;

}
