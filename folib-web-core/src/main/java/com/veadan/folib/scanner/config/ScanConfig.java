package com.veadan.folib.scanner.config;


import lombok.Data;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Data
@Component
public class ScanConfig {

    @Value("${spring.datasource.driver-class-name}")
    private String driverClassName;
    @Value("${spring.datasource.url}")
    private String dbUrl;
    @Value("${spring.datasource.username}")
    private String dbUser;
    @Value("${spring.datasource.password}")
    private String dbPass;
    @Value("${scan.base-proxy}")
    private String scanPoxy;
    //@Value("${scan.proxy-host}")
    //private String poxyHost;
    //@Value("${scan.proxy-port}")
    //private int poxyPort;
    //@Value("${scan.proxy-username}")
    //private String proxyUsername;
    //@Value("${scan.proxy-password}")
    //private String proxyPassword;
    //@Value("${scan.no-proxy}")
    private String noProxy;

    @Value("${scan.storage-base}")
    private String watchMonitorPath;



}
