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
public class LayerManifest {

    /**
     * mediaType
     */
    private String mediaType;

    /**
     * size
     */
    private Long size;

    /**
     * digest
     */
    private String digest;

    /**
     * urls
     */
    private List<String> urls;
}
