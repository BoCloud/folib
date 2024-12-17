package com.veadan.folib.config;

import com.veadan.folib.interceptors.PermissionCheckInterceptor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurerAdapter;

@Configuration
public class FolibWhiteListInterceptorConfig extends WebMvcConfigurerAdapter {

    @Autowired
    private PermissionCheckInterceptor permissionCheckInterceptor;

    @Override
    public void addInterceptors(InterceptorRegistry registry) {
        registry.addInterceptor(permissionCheckInterceptor).addPathPatterns("/**");
        super.addInterceptors(registry);
    }
}
