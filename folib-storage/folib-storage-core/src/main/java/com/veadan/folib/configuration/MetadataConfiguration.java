package com.veadan.folib.configuration;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.beans.BeanUtils;

import javax.annotation.concurrent.Immutable;
import java.io.Serializable;

/**
 * @author leipenghui
 */
@Immutable
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class MetadataConfiguration
        implements Serializable {

    /**
     * 元数据类型
     *
     * @see com.veadan.folib.enums.ArtifactMetadataEnum
     */
    private String type;
    /**
     * 前端是否展示
     */
    private Integer viewShow;
    /**
     * 元数据key
     */
    private String key;

    public MetadataConfiguration(MutableMetadataConfiguration mutableMetadataConfiguration) {
        BeanUtils.copyProperties(mutableMetadataConfiguration, this);
    }

}
