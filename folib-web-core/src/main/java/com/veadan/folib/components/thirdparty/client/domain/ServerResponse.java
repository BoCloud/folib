package com.veadan.folib.components.thirdparty.client.domain;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author leipenghui
 * @date 2024/4/22
 **/
@Builder
@Data
@AllArgsConstructor
@NoArgsConstructor
public class ServerResponse {

    /**
     * data
     */
    private String data;

    /**
     * code
     */
    private int code;
}
