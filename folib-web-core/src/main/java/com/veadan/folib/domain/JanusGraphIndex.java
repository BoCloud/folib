package com.veadan.folib.domain;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Set;

/**
 * @author veadan
 **/
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class JanusGraphIndex {

    /**
     * 索引名称列表
     */
    private Set<String> indexNames;
}
