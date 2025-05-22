package com.veadan.folib.config;

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
import com.fasterxml.jackson.module.jaxb.JaxbAnnotationIntrospector;
import com.veadan.folib.mapper.WebObjectMapperSubtypes;
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
