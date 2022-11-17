package com.veadan.folib.vo;

import com.fasterxml.jackson.annotation.JsonPropertyDescription;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author leipenghui
 * @date 2022/11/16
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ContainerConfigurationManifest {

    /**
     * mediaType
     */
    private String mediaType;

    /**
     * size
     */
    private Integer size;

    /**
     * digest
     */
    private String digest;
}
