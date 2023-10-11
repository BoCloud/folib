package com.veadan.folib.domain.adapter.jfrog;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * @author leipenghui
 **/
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class ArtifactSearchResult {

    /**
     * 制品列表
     */
    private List<ArtifactSearchInfo> results;

    /**
     * 分页信息
     */
    private ArtifactSearchRange range;

}
