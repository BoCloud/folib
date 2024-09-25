package com.veadan.folib.controllers.unicom;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

/**
 * @author huayanjun
 * @since 2024-09-20 11:13
 * appCode、appSecret、appName
 */
@Data
@Configuration
@ConfigurationProperties("custom.unicom")
public class UnicomConfig {
    private String appCode="1234";
    private String appSecret="1234";
    private String appName;
    private String verifyUrl="";
    private String userDetailUrl="";
    private String userByProjectUrl="";

}
