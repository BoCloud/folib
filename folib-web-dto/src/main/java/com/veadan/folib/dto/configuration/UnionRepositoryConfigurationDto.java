package com.veadan.folib.dto.configuration;


import com.fasterxml.jackson.annotation.JsonIgnore;
import com.google.common.collect.Sets;
import com.veadan.folib.configuration.MutableUnionRepositoryConfiguration;
import com.veadan.folib.configuration.MutableUnionTargetRepositoryConfiguration;
import com.veadan.folib.configuration.UnionRepositoryConfiguration;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.beans.BeanUtils;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.LinkedHashSet;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * @author leipenghui
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class UnionRepositoryConfigurationDto
        implements Serializable {

    /**
     * 状态 true 启用 false 关闭
     */
    @NotNull(message = "状态不能为空")
    private Boolean enable;
    /**
     * 同步类型 1 制品路径 2 元数据
     */
    @NotNull(message = "同步类型不能为空")
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
    @Valid
    private Set<UnionTargetRepositoryConfigurationDto> unionTargetRepositories;

    public UnionRepositoryConfigurationDto(UnionRepositoryConfiguration unionConfiguration) {
        this.enable = unionConfiguration.getEnable();
        this.syncType = unionConfiguration.getSyncType();
        this.artifactPaths = unionConfiguration.getArtifactPaths();
        this.metadataKey = unionConfiguration.getMetadataKey();
        this.metadataValue = unionConfiguration.getMetadataValue();
        this.unionTargetRepositories = Optional.ofNullable(unionConfiguration.getUnionTargetRepositories()).orElse(Sets.newLinkedHashSet()).stream().map(item -> {
            UnionTargetRepositoryConfigurationDto unionTargetRepositoryConfigurationForm = UnionTargetRepositoryConfigurationDto.builder().build();
            BeanUtils.copyProperties(item, unionTargetRepositoryConfigurationForm);
            return unionTargetRepositoryConfigurationForm;
        }).collect(Collectors.toCollection(LinkedHashSet::new));
    }

    @JsonIgnore()
    public static UnionRepositoryConfigurationDto fromConfiguration(UnionRepositoryConfiguration source) {
        UnionRepositoryConfiguration unionConfiguration = Optional.ofNullable(source).orElse(
                new UnionRepositoryConfiguration(new MutableUnionRepositoryConfiguration())
        );
        return new UnionRepositoryConfigurationDto(unionConfiguration);
    }

    @JsonIgnore()
    public MutableUnionRepositoryConfiguration getMutableUnionRepositoryConfiguration() {
        MutableUnionRepositoryConfiguration mutableUnionRepositoryConfiguration = MutableUnionRepositoryConfiguration.builder()
                .enable(enable).syncType(syncType).artifactPaths(artifactPaths).metadataKey(metadataKey).metadataValue(metadataValue).build();
        mutableUnionRepositoryConfiguration.setUnionTargetRepositories(Optional.ofNullable(unionTargetRepositories).orElse(Sets.newLinkedHashSet()).stream().map(item -> {
            MutableUnionTargetRepositoryConfiguration mutableUnionTargetRepositoryConfiguration = MutableUnionTargetRepositoryConfiguration.builder().build();
            BeanUtils.copyProperties(item, mutableUnionTargetRepositoryConfiguration);
            return mutableUnionTargetRepositoryConfiguration;
        }).collect(Collectors.toCollection(LinkedHashSet::new)));
        return mutableUnionRepositoryConfiguration;
    }
}