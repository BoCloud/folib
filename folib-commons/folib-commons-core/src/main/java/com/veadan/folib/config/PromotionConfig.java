package com.veadan.folib.config;

import lombok.Data;
import lombok.ToString;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.context.properties.ConfigurationProperties;

import javax.annotation.PostConstruct;

/**
 * @author veadan
 * @date 2024/2/21 18:56
 */
@ConfigurationProperties(prefix = "folib.promotion")
@Data
@ToString
@Slf4j
public class PromotionConfig {
    /**
     * 每个session可堆积的任务队列长度
     */
    private int queueSize = 1000;
    /**
     * 每个session可并行晋级的数量
     */
    private int thread = 2;
    /**
     * 切片传输失败重试次数
     */
    private int retryCount = 3;
    /**
     * 制品上传超时时间 秒单位
     */
    private int wsRequestTimoutOfArtifactUpload = 600;
    /**
     * 使用ws发送请求后等待响应的超时时间，秒单位
     */
    private int wsRequestTimout = 5;
    /**
     * 使用ws通道空闲超时时间，当没有数据传输时，达到这个时间会释放连接
     * 如果连接被防火墙强制打断，此时连接无法收发数据，等待60S后，主动关闭连接
     */
    private int wsMaxSessionIdleTimeout = 1000 * 60;
    /**
     * 当WS隧道空闲多少秒后开始发送心跳
     */
    private int wsHeardBeatIdleTime = 1000 * 20;

    @PostConstruct
    public void init() {
        log.info("init PromotionConfig : {}", this);
    }
}
