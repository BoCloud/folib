package com.veadan.folib.configuration;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.Set;

/**
 * @author veadan
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class MutableUnionRepositoryConfiguration
        implements Serializable {

    /**
     * 状态 true 启用 false 关闭
     */
    private Boolean enable;
    /**
     * 同步类型 1 制品路径 2 元数据
     */
    private Integer syncType;

    /**
     * 制品路径
     */
    private Set<String> artifactPaths;

    /**
     * 元数据key
     */
    private String metadataKey;

    /**
     * 元数据value
     */
    private String metadataValue;

    /**
     * 联邦仓库列表
     */
    private Set<MutableUnionTargetRepositoryConfiguration> unionTargetRepositories;

}