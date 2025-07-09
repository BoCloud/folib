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
package com.folib.config;//package com.folib.config;
//
///**
// * @author veadan
// * @date 2023/4/16
// **/
//
//import org.apache.commons.lang3.StringUtils;
//import org.redisson.Redisson;
//import org.redisson.api.RedissonClient;
//import org.redisson.config.Config;
//import org.redisson.config.SingleServerConfig;
//import org.springframework.beans.factory.annotation.Value;
//import org.springframework.context.annotation.Bean;
//import org.springframework.context.annotation.Configuration;
//
//@Configuration
//public class RedissonSpringDataConfig {
//
//    @Value("${spring.redis.host}")
//    private String host;
//
//    @Value("${spring.redis.port}")
//    private String port;
//
//    @Value("${spring.redis.password}")
//    private String password;
//
//    @Bean(destroyMethod = "shutdown")
//    public RedissonClient redissonClient() {
//        Config config = new Config();
//        SingleServerConfig singleServerConfig = config.useSingleServer().setAddress("redis://" + host + ":" + port);
//        if (StringUtils.isNotBlank(password)) {
//            singleServerConfig.setPassword(password);
//        }
//        System.out.println("------------- redisson -----------------------");
//        System.out.println(config.getTransportMode());
//        ;
////        config.setTransportMode(TransportMode.EPOLL);
//        return Redisson.create(config);
//    }
//
//}
