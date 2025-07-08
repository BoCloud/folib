package com.folib.domain;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * @author veadan
 * @date 2023/12/25
 **/
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class ArtifactSort {

    /**
     * 排序 desc asc
     */
    private String order;

    /**
     * 排序字段列表
     */
    private List<String> keyList;
}
