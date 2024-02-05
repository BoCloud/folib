package com.veadan.folib.domain;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * @author leipenghui
 * @date 2023/12/25
 **/
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class ArtifactPage {

    /**
     * offset
     */
    private Long offset;

    /**
     * limit
     */
    private Long limit;
}
