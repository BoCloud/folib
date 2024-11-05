package com.veadan.folib.scanner.config;


import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

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


    @Value("${scan.storage-base}")
    private String watchMonitorPath;

    public String getWatchMonitorPath() {
        return watchMonitorPath;
    }

    public void setWatchMonitorPath(String watchMonitorPath) {
        this.watchMonitorPath = watchMonitorPath;
    }

    public String getDbUrl() {
        return dbUrl;
    }

    public void setDbUrl(String dbUrl) {
        this.dbUrl = dbUrl;
    }

    public String getDbUser() {
        return dbUser;
    }

    public void setDbUser(String dbUser) {
        this.dbUser = dbUser;
    }

    public String getDbPass() {
        return dbPass;
    }

    public void setDbPass(String dbPass) {
        this.dbPass = dbPass;
    }

    public String getScanPoxy() {
        return scanPoxy;
    }

    public void setScanPoxy(String scanPoxy) {
        this.scanPoxy = scanPoxy;
    }

    public String getDriverClassName() {
        return driverClassName;
    }

    public void setDriverClassName(String driverClassName) {
        this.driverClassName = driverClassName;
    }
}
