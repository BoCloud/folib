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
package com.folib.utils;


import com.fasterxml.jackson.core.TreeNode;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.io.File;
import java.io.InputStream;
import java.io.Reader;
import java.net.URL;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.stream.Collectors;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import com.folib.exception.JsonSensitiveDataException;
import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class MapperUtilsBase {
    private final ObjectMapper mapper;

    private static final Logger log = LoggerFactory.getLogger(MapperUtilsBase.class);

    protected MapperUtilsBase(ObjectMapper mapper) {
        this.mapper = mapper;
        this.mapper.findAndRegisterModules();
    }

    @Nonnull
    public String valueToString(@Nullable Object value) {
        return valueToString(value, false);
    }

    @Nonnull
    public String valueToString(@Nullable Object value, boolean prettyPrint) {
        return unchecked(() -> prettyPrint ? this.mapper.writerWithDefaultPrettyPrinter().writeValueAsString(value) : this.mapper.writeValueAsString(value));
    }

    @Nonnull
    public JsonNode valueToTree(@Nullable Object value) {
        return unchecked(() -> this.mapper.valueToTree(value));
    }

    public <T> T treeToValue(JsonNode root, Class<T> clazz) {
        return unchecked(() -> this.mapper.treeToValue((TreeNode)root, clazz));
    }

    @Nonnull
    public JsonNode readTree(@Nullable String value) {
        return unchecked(() -> this.mapper.readTree(value));
    }

    @Nonnull
    public JsonNode readTree(@Nullable File value) {
        return unchecked(() -> this.mapper.readTree(value));
    }

    @Nonnull
    public JsonNode readTree(@Nullable byte[] value) {
        return unchecked(() -> this.mapper.readTree(value));
    }

    public ObjectNode createObjectNode() {
        return this.mapper.createObjectNode();
    }

    public byte[] valueToByteArray(Object value) {
        return unchecked(() -> this.mapper.writerWithDefaultPrettyPrinter().writeValueAsBytes(value));
    }

    public String writeValueAsString(Object value) {
        return unchecked(() -> this.mapper.writeValueAsString(value));
    }

    public void writeValue(File file, Object value) {
        writeValue(file, value, false);
    }

    public void writeValue(File file, Object value, boolean prettyPrint) {
        unchecked(() -> {
            if (prettyPrint) {
                this.mapper.writerWithDefaultPrettyPrinter().writeValue(file, value);
            } else {
                this.mapper.writeValue(file, value);
            }
            return null;
        });
    }

    @Nonnull
    public <T> T readValue(@Nonnull String object, @Nonnull Class<T> clazz) {
        return unchecked(() -> this.mapper.readValue(object, clazz));
    }

    @Nonnull
    public <T> T readValue(@Nonnull InputStream stream, @Nonnull Class<T> clazz) {
        return unchecked(() -> this.mapper.readValue(stream, clazz));
    }

    @Nonnull
    public <T> T readValue(String json, TypeReference<T> clazz) {
        return unchecked(() -> this.mapper.readValue(json, clazz));
    }

    @Nonnull
    public <T> T readValueRef(String json, TypeReference<T> clazz) {
        return unchecked(() -> this.mapper.readValue(json, clazz));
    }

    @Nonnull
    public <T> T readValueRef(File file, TypeReference<T> clazz) {
        return unchecked(() -> this.mapper.readValue(file, clazz));
    }

    @Nonnull
    public <T> T readValue(@Nonnull Reader reader, @Nonnull Class<T> clazz) {
        return unchecked(() -> this.mapper.readValue(reader, clazz));
    }

    public <T> T clone(@Nonnull T object, @Nonnull Class<T> clazz) {
        return readValue(valueToByteArray(object), clazz);
    }

    @Nonnull
    public <T> T readValue(@Nonnull File file, @Nonnull Class<T> clazz) {
        return unchecked(() -> this.mapper.readValue(file, clazz));
    }

    @Nonnull
    public <T> T readValue(@Nonnull URL url, @Nonnull Class<T> clazz) {
        return unchecked(() -> this.mapper.readValue(url, clazz));
    }

    @Nonnull
    public <T> T readValue(@Nonnull byte[] bytes, @Nonnull Class<T> clazz) {
        return unchecked(() -> this.mapper.readValue(bytes, clazz));
    }

    @Nullable
    public Long asLong(@Nullable Object value) {
        if (value == null)
            return null;
        if (value instanceof Long)
            return (Long)value;
        if (value instanceof Integer)
            return Long.valueOf(((Integer)value).longValue());
        if (value instanceof String)
            return Long.valueOf((String)value);
        throw new IllegalArgumentException("Unexpected value type: " + value.getClass());
    }

    private <T> T unchecked(Callable<T> callable) {
        try {
            return callable.call();
        } catch (Exception e) {
            throw new JsonSensitiveDataException(e);
        }
    }

    public String merge(String original, String additionalData) {
        Map<String, Object> originalMap = readValue(original, Map.class);
        Map<String, Object> additionalDataMap = readValue(additionalData, Map.class);
        return valueToString(merge(originalMap, additionalDataMap));
    }

    public static Map<String, Object> merge(Map<String, Object> original, Map<String, Object> additionalData) {
        Set<String> allKeys = new HashSet<>(original.keySet());
        allKeys.addAll(additionalData.keySet());
        return (Map<String, Object>)allKeys.stream()
                .map(key -> {
                    Object prevValue = original.get(key);
                    if (additionalData.containsKey(key)) {
                        Object newValue = additionalData.get(key);
                        return (newValue == null) ? Pair.of(key, null) : ((newValue instanceof Map && prevValue instanceof Map) ? Pair.of(key, merge((Map<String, Object>)prevValue, (Map<String, Object>)newValue)) : Pair.of(key, newValue));
                    }
                    return Pair.of(key, prevValue);
                }).filter(pair -> (pair.getValue() != null))
                .collect(Collectors.toMap(Pair::getLeft, Pair::getRight));
    }

    public <T> List<T> jsonStringToObjectArray(String content, Class<T> clazz) {
        return unchecked(() -> (List)this.mapper.readValue(content, (JavaType)this.mapper.getTypeFactory().constructCollectionType(List.class, clazz)));
    }

    public boolean isEqual(String content1, String content2) {
        try {
            JsonNode yaml1AsJson = readTree(content1);
            JsonNode yaml2AsJson = readTree(content2);
            return yaml1AsJson.equals(yaml2AsJson);
        } catch (Exception e) {
            log.warn("Unable to parse configuration with error - {}", e.getMessage());
            log.debug("", e);
            return false;
        }
    }

    protected ObjectMapper getMapper() {
        return this.mapper;
    }
}

