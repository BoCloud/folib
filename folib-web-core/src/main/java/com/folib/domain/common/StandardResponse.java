package com.folib.domain.common;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author veadan
 * @date 2024/6/14
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class StandardResponse {

    private String code;

    private String message;

    private StandardError error;
}
