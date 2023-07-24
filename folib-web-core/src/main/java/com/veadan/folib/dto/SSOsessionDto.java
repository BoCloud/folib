package com.veadan.folib.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class SSOsessionDto {
    private String session_state;
    private  String code;
    private  String grant_type="authorization_code";
    private  String client_id;
    private  String redirect_uri;
    private  String access_token_url;

}
