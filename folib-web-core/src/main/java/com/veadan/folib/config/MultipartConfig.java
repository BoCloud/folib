package com.veadan.folib.config;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.web.servlet.MultipartConfigFactory;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.util.unit.DataSize;

import javax.servlet.MultipartConfigElement;

@Configuration
public class MultipartConfig {
    @Value("${folib.multipart.maxFileSize:2048}")
    private String maxFileSize;
    @Value("${folib.multipart.maxRequestSize:2048}")
    private String maxRequestSize;

    /**
     * 配置上传文件大小的配置
     */
    @Bean
    public MultipartConfigElement multipartConfigElement() {
        MultipartConfigFactory factory = new MultipartConfigFactory();
        //  单个数据大小
        factory.setMaxFileSize(DataSize.ofMegabytes(Long.parseLong(maxFileSize)));
        /// 总上传数据大小
        factory.setMaxRequestSize(DataSize.ofMegabytes(Long.parseLong(maxRequestSize)));
        return factory.createMultipartConfig();
    }
}