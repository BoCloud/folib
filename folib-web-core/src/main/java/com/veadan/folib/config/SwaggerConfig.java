package com.veadan.folib.config;

import java.util.Collections;

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

@Configuration
@EnableSwagger2
public class SwaggerConfig
{

    @Value("${folib.version}")
    public String folibVersion;

    @Bean
    public Docket folibApiDocket()
    {
        Contact contact = new Contact("Folib",
                                      "http://github.com/folib/folib/",
                                      "folib-dev@veadan.com");
        ApiInfo apiInfo = new ApiInfo("Veadan Team & Development Ltd.",
                                      "This is the documentation of Folib's REST API.",
                                      folibVersion,
                                      "http://github.com/folib/folib/",
                                      contact,
                                      "Apache 2.0",
                                      "http://www.apache.org/licenses/LICENSE-2.0.html",
                                      Collections.EMPTY_LIST);
        
        return new Docket(DocumentationType.SWAGGER_2).protocols(Sets.newHashSet("http", "https"))
                                                      .pathMapping("/")
                                                      .apiInfo(apiInfo);
    }

    @Bean
    public UiConfiguration uiConfiguration()
    {
        return UiConfigurationBuilder.builder().build();
    }
    
}
