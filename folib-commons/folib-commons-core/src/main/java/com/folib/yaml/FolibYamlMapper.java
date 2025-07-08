package com.folib.yaml;

import javax.annotation.Nonnull;
import java.util.*;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonTypeName;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.introspect.JacksonAnnotationIntrospector;
import com.fasterxml.jackson.databind.jsontype.NamedType;
import com.fasterxml.jackson.databind.module.SimpleModule;
import com.fasterxml.jackson.dataformat.yaml.YAMLGenerator;
import com.fasterxml.jackson.dataformat.yaml.YAMLMapper;

/**
 * @author Veadan
 */
public class FolibYamlMapper
        extends YAMLMapper
{

    public FolibYamlMapper(@Nonnull final Set<Class<?>> contextClasses)
    {
        enable(SerializationFeature.WRAP_ROOT_VALUE);
        enable(DeserializationFeature.UNWRAP_ROOT_VALUE);
        disable(YAMLGenerator.Feature.USE_NATIVE_TYPE_ID);
        setAnnotationIntrospector(new JacksonAnnotationIntrospector());
        setSerializationInclusion(JsonInclude.Include.NON_NULL);

        contextClasses.forEach(
                contextClass -> registerSubtypes(new NamedType(contextClass, Optional.ofNullable(
                        contextClass.getAnnotation(JsonTypeName.class)).map(JsonTypeName::value).orElse(null))));

        registerModules();
    }

    private void registerModules()
    {
        SimpleModule simpleModule = new SimpleModule();
        simpleModule.addAbstractTypeMapping(Map.class, LinkedHashMap.class);
        simpleModule.addAbstractTypeMapping(Set.class, LinkedHashSet.class);
        this.registerModule(simpleModule);
    }
}
