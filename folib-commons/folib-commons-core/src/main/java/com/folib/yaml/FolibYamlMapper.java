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
