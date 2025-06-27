package com.veadan.folib.components.thirdparty.foeyes.reponse;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author veadan
 * @date 2024/4/22
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ProjectInfo {

    /**
     * uuid
     */
    private String uuid;

    /**
     * name
     */
    private String name;

    /**
     * classifier
     */
    private String classifier;

    /**
     * version
     */
    private String version;

    /**
     * parentProject
     */
    private Boolean parentProject;

    /**
     * metrics
     */
    private Metrics metrics;
}
