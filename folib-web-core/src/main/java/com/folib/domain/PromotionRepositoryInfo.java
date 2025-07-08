package com.folib.domain;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 节点之间的晋级
 *
 * @author veadan
 */
@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
public class PromotionRepositoryInfo {

    /**
     * 源存储空间
     */
    private String sourceStorageId;

    /**
     * 源仓库
     */
    private String sourceRepositoryId;

    /**
     * 源地址
     */
    private String sourceBaseUrl;

    /**
     * 源制品路径
     */
    private String sourceArtifactPath;

    /**
     * 目标存储空间
     */
    private String targetStorageId;

    /**
     * 目标仓库
     */
    private String targetRepositoryId;

    /**
     * 目标地址
     */
    private String targetBaseUrl;

    /**
     * 目标制品路径
     */
    private String targetArtifactPath;
}
