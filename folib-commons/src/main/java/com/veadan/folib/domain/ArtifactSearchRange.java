package com.veadan.folib.domain;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author leipenghui
 **/
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class ArtifactSearchRange {

    /**
     * 开始
     */
    @JsonProperty(value = "start_pos")
    private Long startPos;

    /**
     * 结束
     */
    @JsonProperty(value = "end_pos")
    private Long endPos;

    /**
     * 总数
     */
    private Long total;

    /**
     * limit
     */
    private Long limit;

}
