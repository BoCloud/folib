package com.folib.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author veadan
 * @date 2022/11/16
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ArtifactCoordinates {

    /**
     * 组织
     */
    private String groupId;
    /**
     * 包名称
     */
    private String  artifactId;
    /**
     * 扩展名
     */
    private String  extension;
    /**
     * 版本
     */
    private String version;
    /**
     * classifier
     */
    private String  classifier;
}
