//package com.veadan.folib.config.redis;
//
//import com.alibaba.fastjson.JSONObject;
//import lombok.extern.slf4j.Slf4j;
//import org.apache.commons.lang3.StringUtils;
//import org.redisson.Redisson;
//import org.redisson.api.RedissonClient;
//import org.redisson.client.codec.StringCodec;
//import org.redisson.config.ClusterServersConfig;
//import org.redisson.config.Config;
//import org.redisson.config.SingleServerConfig;
//import org.springframework.beans.factory.annotation.Autowired;
//import org.springframework.context.annotation.Bean;
//import org.springframework.context.annotation.Conditional;
//import org.springframework.context.annotation.Configuration;
//
//import java.util.Objects;
//
///**
// * @author leipenghui
// **/
//@Slf4j
//@Configuration
//public class RedisConfig {
//
//    @Autowired(required = false)
//    private RedisConfigProperties redisConfigProperties;
//
//    /**
//     * 所有对Redisson的使用都是通过RedissonClient对象
//     *
//     * @return redissonClient
//     */
//    @Bean(destroyMethod = "shutdown")
//    @Conditional(RedisCondition.class)
//    public RedissonClient redissonClient() {
//        if (Objects.nonNull(redisConfigProperties)) {
//            log.info("RedisConfigProperties：{}", JSONObject.toJSONString(redisConfigProperties));
//            String[] clusterNodes = new String[redisConfigProperties.getCluster().getNodeAddresses().size()];
//            for (int i = 0; i < clusterNodes.length; i++) {
//                clusterNodes[i] = redisConfigProperties.getCluster().getNodeAddresses().get(i);
//            }
//            Config config = new Config();
//            //对象编码选择纯字符串编码
//            config.setCodec(StringCodec.INSTANCE);
//            if (clusterNodes.length == 1) {
//                SingleServerConfig singleServerConfig = config.useSingleServer().setAddress(clusterNodes[0]);
//                //心跳检测，定时与redis连接，可以防止一段时间过后，与redis的连接断开
//                singleServerConfig.setPingConnectionInterval(10000);
//                if (StringUtils.isNotBlank(redisConfigProperties.getPassword())) {
//                    singleServerConfig.setPassword(redisConfigProperties.getPassword());
//                }
//            } else {
//                ClusterServersConfig clusterServersConfig = config.useClusterServers()
//                        .addNodeAddress(clusterNodes);
//                //心跳检测，定时与redis连接，可以防止一段时间过后，与redis的连接断开
//                clusterServersConfig.setPingConnectionInterval(10000);
//                if (StringUtils.isNotBlank(redisConfigProperties.getPassword())) {
//                    clusterServersConfig.setPassword(redisConfigProperties.getPassword());
//                }
//            }
//            return Redisson.create(config);
//        }
//        return null;
//    }
//}
