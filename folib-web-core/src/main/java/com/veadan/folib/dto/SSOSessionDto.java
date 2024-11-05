package com.veadan.folib.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author leipenghui
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
public class SSOSessionDto {

    private String sessionState;
    private String code;
    private String grantType = "authorization_code";
    private String clientId;
    private String redirectUri;
    private String accessTokenUrl;

}
