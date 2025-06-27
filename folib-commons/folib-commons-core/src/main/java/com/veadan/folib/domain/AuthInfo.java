package com.veadan.folib.domain;

import com.alibaba.fastjson.annotation.JSONField;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author veadan
 * @date 2024/1/25
 **/
@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
public class AuthInfo {

    @JSONField(name = "expires_in")
    private Long expiresIn;

    private String token;
}
