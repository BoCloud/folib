package com.folib.domain;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Map;

/**
 * @author veadan
 * @date 2024/6/13
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class DockerSubsidiary {

    /**
     * path
     */
    private String path;

    /**
     * name
     */
    private String name;

    /**
     * url
     */
    private String url;
}
