package com.veadan.folib.controllers.unicom;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

import java.util.Map;

/**
 * @author huayanjun
 * @since 2024-09-20 11:13
 * appCode、appSecret、appName
 */
@Data
@Configuration
@ConfigurationProperties("custom.unicom")
public class UnicomConfig {
    private String appCode;
    private String appSecret;
    private String appName;
    private String verifyUrl;
    private String userDetailUrl;
    // 发送邮件地址
    private String sendEmailUrl;
    private String redirectUrl;
    // 用户详情
    private String userInfoUrl;

}
