package com.veadan.folib.domain.adapter.jfrog;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author veadan
 **/
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class ArtifactDockerQuery {

    /**
     * view
     */
    private String view;

    /**
     * 仓库
     */
    private String repoKey;

    /**
     * 制品路径
     */
    private String path;

}
