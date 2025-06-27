//package com.veadan.folib.config.redis;
//
//import lombok.Data;
//import org.springframework.boot.context.properties.ConfigurationProperties;
//import org.springframework.stereotype.Component;
//
//import java.io.Serializable;
//
///**
// * @author veadan
// **/
//@Data
//@Component
//@ConfigurationProperties(prefix = "spring.redis")
//public class RedisConfigProperties implements Serializable {
//
//    private static final long serialVersionUID = 1L;
//
//    private String password;
//
//    private Cluster cluster;
//
//
//    public String getPassword() {
//        return password;
//    }
//
//    public void setPassword(String password) {
//        this.password = password;
//    }
//
//    public Cluster getCluster() {
//        return cluster;
//    }
//
//    public void setCluster(Cluster cluster) {
//        this.cluster = cluster;
//    }
//}
//
