package com.folib.components.thirdparty.foeyes.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author veadan
 * @date 2024/4/22
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class TagRequest {

    /**
     * id
     */
    private Long id;

    /**
     * name
     */
    private String name;
}
