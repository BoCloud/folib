package com.folib.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * @author veadan
 * @date 2022/11/23
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class SearchArtifactPage {

    /**
     * 制品列表
     */
    private List<SearchArtifactInfo> artifactInfoList;

    /**
     * 总数
     */
    private Long total;
}
