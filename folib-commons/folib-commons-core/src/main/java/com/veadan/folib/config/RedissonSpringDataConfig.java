package com.veadan.folib.config;//package com.veadan.folib.config;
//
///**
// * @author leipenghui
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
