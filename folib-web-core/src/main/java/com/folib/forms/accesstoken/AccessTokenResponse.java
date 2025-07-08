package com.folib.forms.accesstoken;

import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Data;

import java.util.Date;

/**
 * @author huayanjun
 * @since 2024-08-20 14:24
 */
@Data
public class AccessTokenResponse {
    private String tokenId;
    private String jwt;
    private String userName;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date exp;


}
