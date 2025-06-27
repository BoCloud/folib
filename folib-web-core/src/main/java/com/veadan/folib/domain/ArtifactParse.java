package com.veadan.folib.domain;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author veadan
 * @date 2023/4/2
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ArtifactParse {
    /**
     * 类型 1 解析成功 2 解析失败
     */
    private Integer type;
    /**
     * filePath
     */
    private String filePath;
    /**
     * groupId
     */
    private String groupId;

    /**
     * artifactId
     */
    private String artifactId;

    /**
     * version
     */
    private String version;

    /**
     * classifier
     */
    private String classifier;
}
