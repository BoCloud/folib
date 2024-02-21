package com.veadan.folib.config;

import lombok.Data;
import lombok.ToString;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.context.properties.ConfigurationProperties;

import javax.annotation.PostConstruct;

/**
 * @author pengYongQiang
 * @date 2024/2/21 18:56
 */
@ConfigurationProperties(prefix = "folib.promotion")
@Data
@ToString
@Slf4j
public class PromotionConfig {
    /**
     * 每个seesion可堆积的任务队列长度
     */
    private int queueSize = 1000;
    /**
     * 切片传输失败重试次数
     */
    private int retryCount = 3;
    /**
     * 制品上传超时时间 秒单位
     */
    private int wsRequestTimoutOfArtifactUpload = 600;
    /**
     * 使用ws发送请求的超时时间
     */
    private int wsRequestTimout = 5;

    @PostConstruct
    public void init() {
        log.info("init PromotionConfig : {}", this);
    }
}
