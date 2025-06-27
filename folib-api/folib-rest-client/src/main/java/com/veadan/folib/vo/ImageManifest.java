package com.veadan.folib.vo;

import com.fasterxml.jackson.annotation.JsonPropertyDescription;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * @author veadan
 * @date 2022/11/16
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ImageManifest {

    /**
     * schemaVersion
     */
    private Integer schemaVersion;

    /**
     * mediaType
     */
    private String mediaType;

    /**
     * 配置信息
     */
    private ContainerConfigurationManifest config;

    /**
     * 层级信息
     */
    private List<LayerManifest> layers;
}
