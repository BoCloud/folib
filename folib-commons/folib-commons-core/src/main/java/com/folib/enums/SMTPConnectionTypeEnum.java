package com.folib.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @author veadan
 * @date 2022/10/18
 **/
@AllArgsConstructor
@NoArgsConstructor
@Getter
public enum SMTPConnectionTypeEnum {

    /**
     * plain
     */
    PLAIN("plain"),
    /**
     * ssl
     */
    SSL("ssl"),
    /**
     * tls
     */
    TLS("tls"),
    ;

    private String connection;

}
