package com.folib.components.sbom;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author veadan
 * @date 2024/10/30
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class License {

    /**
     * 名称
     */
    private String id;
}
