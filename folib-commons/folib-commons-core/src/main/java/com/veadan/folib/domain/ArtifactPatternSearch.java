package com.veadan.folib.domain;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * @author veadan
 **/
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class ArtifactPatternSearch {

    /**
     * 仓库地址
     */
    private String repoUri;

    /**
     * 搜索pattern
     */
    private String sourcePattern;

    /**
     * 制品列表
     */
    private List<String> files;

}
