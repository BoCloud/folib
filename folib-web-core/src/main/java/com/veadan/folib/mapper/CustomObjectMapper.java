package com.veadan.folib.mapper;

import java.util.Optional;

import javax.annotation.PostConstruct;

import com.fasterxml.jackson.databind.MapperFeature;
import org.springframework.context.annotation.Primary;
import org.springframework.stereotype.Component;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.fasterxml.jackson.databind.AnnotationIntrospector;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.introspect.JacksonAnnotationIntrospector;
import com.fasterxml.jackson.databind.jsontype.NamedType;
import com.fasterxml.jackson.databind.type.TypeFactory;
import com.fasterxml.jackson.module.jaxb.JaxbAnnotationIntrospector;

/**
 * @author 
 */
//@Component
//@Primary
public class CustomObjectMapper
        extends ObjectMapper
{

    @PostConstruct
    public void postConstruct()
    {
        disable(SerializationFeature.FAIL_ON_EMPTY_BEANS);
        enable(MapperFeature.ACCEPT_CASE_INSENSITIVE_ENUMS);

        final AnnotationIntrospector jaxbIntrospector = new JaxbAnnotationIntrospector(TypeFactory.defaultInstance());
        final AnnotationIntrospector jacksonIntrospector = new JacksonAnnotationIntrospector();
        AnnotationIntrospector introspector = AnnotationIntrospector.pair(jacksonIntrospector, jaxbIntrospector);
        setAnnotationIntrospector(introspector);

        WebObjectMapperSubtypes.INSTANCE.subtypes()
                                        .stream()
                                        .forEach(contextClass -> registerSubtypes(new NamedType(contextClass,
                                                                                             Optional.ofNullable(
                                                                                                     contextClass.getAnnotation(
                                                                                                             JsonTypeName.class))
                                                                                                     .map(JsonTypeName::value)
                                                                                                     .orElse(null))));
    }
}
