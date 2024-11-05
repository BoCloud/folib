package com.veadan.folib.components.sbom;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author leipenghui
 * @date 2024/10/30
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class BomHash {

    /**
     * 算法
     */
    private String alg;

    /**
     * 内容
     */
    private String content;
}
