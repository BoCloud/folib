package com.veadan.folib.config.webdav;


import io.milton.servlet.MiltonServlet;
import org.springframework.boot.web.servlet.ServletRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * @author huayanjun
 * @since 2025-03-09 16:18
 */
@Configuration
public class WebdavConfig {
    @Bean
    public ServletRegistrationBean<MiltonServlet> miltonServlet() {
        MiltonServlet servlet = new MiltonServlet();
        ServletRegistrationBean<MiltonServlet> bean = new ServletRegistrationBean<>(servlet, "/dav/*");
        bean.setLoadOnStartup(1);
        bean.addInitParameter("resource.factory.class", FolibResourceFactory.class.getName());
        bean.addInitParameter("milton.exclude.methods", "GET,POST,PUT,DELETE");
        return bean;
    }

}
