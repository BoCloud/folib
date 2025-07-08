package com.folib.config;

import cn.hutool.core.util.RandomUtil;
import com.google.common.collect.Sets;
import io.swagger.model.ApiInfo;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.PathItem;
import org.springdoc.core.customizers.GlobalOpenApiCustomizer;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import cn.hutool.core.util.RandomUtil;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.info.License;
import org.springdoc.core.customizers.GlobalOpenApiCustomizer;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

@Configuration
//@EnableSwagger2
public class SwaggerConfig {

    @Value("${folib.version}")
    public String folibVersion;

    @Value("${swagger.enable}")
    private Boolean enable;

    //@Bean
    //public Docket folibApiDocket() {
    //    Contact contact = new Contact("Folib",
    //            "http://folib.com",
    //            "folib-dev@veadan.com");
    //    ApiInfo apiInfo = new ApiInfo("Bocloud-牧品团队提供服务",
    //            "这是Folib制品库默认的对外开放的所有API（需要通过token访问）",
    //            folibVersion,
    //            "http://folib.com",
    //            contact,
    //            "",
    //            "",
    //            Collections.EMPTY_LIST);
    //
    //    return new Docket(DocumentationType.SWAGGER_2).protocols(Sets.newHashSet("http", "https"))
    //            .pathMapping("/")
    //            .enable(enable)
    //            .apiInfo(apiInfo);
    //}
    //
    //@Bean
    //public UiConfiguration uiConfiguration() {
    //    return UiConfigurationBuilder.builder().build();
    //}

    /**
     * 根据@Tag 上的排序，写入x-order
     *
     * @return the global open api customizer
     */
    @Bean
    public GlobalOpenApiCustomizer orderGlobalOpenApiCustomizer() {
        return openApi -> {
            if (openApi.getTags()!=null){
                openApi.getTags().forEach(tag -> {
                    Map<String,Object> map=new HashMap<>();
                    map.put("x-order", RandomUtil.randomInt(0,100));
                    tag.setExtensions(map);
                });
            }
            if(openApi.getPaths()!=null){
                openApi.addExtension("x-test123","333");
                openApi.getPaths().addExtension("x-abb", RandomUtil.randomInt(1,100));
            }

        };
    }

    @Bean
    public OpenAPI customOpenAPI() {
        return new OpenAPI()
                .path("/", new PathItem())
                .info(new Info()
                        .title("FoLib API")
                        .version(folibVersion)
                        .description( "Bocloud-牧品团队提供服务 这是Folib制品库默认的对外开放的所有API（需要通过token访问）")
                        .termsOfService("http://folib.com"));
    }
}
