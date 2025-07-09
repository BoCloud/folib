/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
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
