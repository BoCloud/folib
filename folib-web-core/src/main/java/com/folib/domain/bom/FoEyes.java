package com.folib.domain.bom;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author veadan
 * @date 2024/4/23
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class FoEyes {

    /**
     * 上传状态
     */
    private String uploadStatus;
}
