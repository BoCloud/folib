/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.config;

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
