package com.folib.model.response;

import lombok.Data;

/**
 * @author huayanjun
 * @since 2025-07-01 11:45
 */
@Data
public class NpmLoginRes {

    private String token;
    private String username;
    private boolean ok;

}
