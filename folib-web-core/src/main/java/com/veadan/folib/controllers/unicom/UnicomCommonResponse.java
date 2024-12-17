package com.veadan.folib.controllers.unicom;

import lombok.Data;

/**
 * @author huayanjun
 * @since 2024-12-11 17:05
 */
@Data
public class UnicomCommonResponse {
    private int code;

    private Object data;

    private String message;

}
