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

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonTypeName;
import com.fasterxml.jackson.databind.AnnotationIntrospector;
import com.fasterxml.jackson.databind.MapperFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.introspect.JacksonAnnotationIntrospector;
import com.fasterxml.jackson.databind.jsontype.NamedType;
import com.fasterxml.jackson.databind.module.SimpleModule;
import com.fasterxml.jackson.databind.ser.std.ToStringSerializer;
import com.fasterxml.jackson.databind.type.TypeFactory;
import com.fasterxml.jackson.datatype.guava.GuavaModule;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.fasterxml.jackson.module.jaxb.JaxbAnnotationIntrospector;
import com.folib.mapper.WebObjectMapperSubtypes;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import org.springframework.http.converter.json.Jackson2ObjectMapperBuilder;

import java.util.Optional;

@Configuration
public class JacksonConfig {

    @Bean
    @Primary
    public ObjectMapper customObjectMapper(Jackson2ObjectMapperBuilder builder) {
        // 创建并配置 ObjectMapper
        ObjectMapper objectMapper = builder
                .serializationInclusion(JsonInclude.Include.NON_NULL) // 忽略 null 值
                .featuresToDisable(SerializationFeature.FAIL_ON_EMPTY_BEANS) // 禁用空对象错误
                .featuresToEnable(MapperFeature.ACCEPT_CASE_INSENSITIVE_ENUMS) // 启用大小写不敏感枚举
                .featuresToEnable(MapperFeature.DEFAULT_VIEW_INCLUSION)
                .featuresToEnable(SerializationFeature.FAIL_ON_SELF_REFERENCES)
                .build();
        objectMapper.registerModule(new GuavaModule());
        objectMapper.registerModule(new JavaTimeModule());  // 支持 Java 8 日期时间
        // 配置 AnnotationIntrospector
        AnnotationIntrospector jaxbIntrospector = new JaxbAnnotationIntrospector(TypeFactory.defaultInstance());
        AnnotationIntrospector jacksonIntrospector = new JacksonAnnotationIntrospector();
        objectMapper.setAnnotationIntrospector(AnnotationIntrospector.pair(jacksonIntrospector, jaxbIntrospector));
        // 将 Long 自动转为 String
        SimpleModule module = new SimpleModule();
        module.addSerializer(Long.class, new ToStringSerializer());
        objectMapper.registerModule(module);
        // 注册子类型
        WebObjectMapperSubtypes.INSTANCE.subtypes().forEach(contextClass ->
                objectMapper.registerSubtypes(new NamedType(contextClass,
                        Optional.ofNullable(contextClass.getAnnotation(JsonTypeName.class))
                                .map(JsonTypeName::value)
                                .orElse(null)))
        );

        return objectMapper;
    }
}
