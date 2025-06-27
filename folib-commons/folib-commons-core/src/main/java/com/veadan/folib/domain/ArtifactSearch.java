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
public class ArtifactSearch<T> {

    /**
     * 制品列表
     */
    private List<T> results;

    /**
     * 分页信息
     */
    private ArtifactSearchRange range;

}
