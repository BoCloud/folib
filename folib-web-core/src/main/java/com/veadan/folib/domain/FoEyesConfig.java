package com.veadan.folib.domain;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author veadan
 * @date 2024/4/25
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class FoEyesConfig {

    /**
     * 是否启用
     */
    private Boolean enable;

    /**
     * accessKey
     */
    private String accessKey;
}
