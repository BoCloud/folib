package com.veadan.folib.config;

import com.google.common.collect.Sets;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import springfox.documentation.service.ApiInfo;
import springfox.documentation.service.Contact;
import springfox.documentation.spi.DocumentationType;
import springfox.documentation.spring.web.plugins.Docket;
import springfox.documentation.swagger.web.UiConfiguration;
import springfox.documentation.swagger.web.UiConfigurationBuilder;
import springfox.documentation.swagger2.annotations.EnableSwagger2;

import java.util.Collections;

@Configuration
@EnableSwagger2
public class SwaggerConfig {

    @Value("${folib.version}")
    public String folibVersion;

    @Value("${swagger.enable}")
    private Boolean enable;

    @Bean
    public Docket folibApiDocket() {
        Contact contact = new Contact("Folib",
                "http://folib.com",
                "folib-dev@veadan.com");
        ApiInfo apiInfo = new ApiInfo("Bocloud-牧品团队提供服务",
                "这是Folib制品库默认的对外开放的所有API（需要通过token访问）",
                folibVersion,
                "http://folib.com",
                contact,
                "",
                "",
                Collections.EMPTY_LIST);

        return new Docket(DocumentationType.SWAGGER_2).protocols(Sets.newHashSet("http", "https"))
                .pathMapping("/")
                .enable(enable)
                .apiInfo(apiInfo);
    }

    @Bean
    public UiConfiguration uiConfiguration() {
        return UiConfigurationBuilder.builder().build();
    }
}
