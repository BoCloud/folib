package com.folib.configuration;

import com.folib.enums.ArtifactMetadataEnum;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

/**
 * @author veadan
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class MutableMetadataConfiguration
        implements Serializable {

    /**
     * 元数据类型
     *
     * @see ArtifactMetadataEnum
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

}
