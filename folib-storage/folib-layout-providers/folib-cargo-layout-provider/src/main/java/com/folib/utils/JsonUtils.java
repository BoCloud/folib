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

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.TreeNode;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.github.fge.jsonpatch.mergepatch.JsonMergePatch;

import java.io.IOException;
import java.io.InputStream;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.function.Consumer;
import javax.annotation.Nullable;

import com.folib.exception.JsonMergeException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class JsonUtils extends MapperUtilsBase {
    private static final Logger log = LoggerFactory.getLogger(JsonUtils.class);

    // 使用静态初始化块来确保实例化时正确配置 ObjectMapper
    private static final JsonUtils instance;
    private static final JsonUtils instanceWithAll;

    static {
        ObjectMapper mapperNonNull = new ObjectMapper();
        mapperNonNull.setSerializationInclusion(JsonInclude.Include.NON_NULL);
        mapperNonNull.enable(SerializationFeature.INDENT_OUTPUT);
        instance = new JsonUtils(mapperNonNull);

        ObjectMapper mapperAlways = new ObjectMapper();
        mapperAlways.setSerializationInclusion(JsonInclude.Include.ALWAYS);
        mapperNonNull.enable(SerializationFeature.INDENT_OUTPUT);
        instanceWithAll = new JsonUtils(mapperAlways);
    }


    public JsonUtils(ObjectMapper objectMapper) {
        super(objectMapper);
    }

    public static synchronized JsonUtils getInstance() {
        return instance;
    }

    public static synchronized JsonUtils getInstanceSerializingNulls() {
        return instanceWithAll;
    }

    public static String toJsonMerge(Object entity, Set<String> fieldsToInclude) {
        JsonNode tree = getInstanceSerializingNulls().valueToTree(entity);
        for (Iterator<Map.Entry<String, JsonNode>> i = tree.fields(); i.hasNext(); ) {
            Map.Entry entry = i.next();
            if (!fieldsToInclude.contains(entry.getKey()))
                i.remove();
        }
        return getInstanceSerializingNulls().valueToString(tree);
    }

    public static synchronized JsonUtils createInstance(ObjectMapper objectMapper) {
        return new JsonUtils(objectMapper);
    }

    public static <I, T extends I> I jsonMerge(I originalModel, String patch, Class<T> modelType, @Nullable Consumer<JsonNode> validatePatchResult) {
        ObjectMapper mapper = (new ObjectMapper()).enable(SerializationFeature.INDENT_OUTPUT);
        try {
            JsonNode entity = mapper.valueToTree(originalModel);
            JsonMergePatch patchTool = (JsonMergePatch) mapper.readValue(patch, JsonMergePatch.class);
            JsonNode modifiedEntity = patchTool.apply(entity);
            if (validatePatchResult != null)
                validatePatchResult.accept(modifiedEntity);
            return (I) mapper.treeToValue((TreeNode) modifiedEntity, modelType);
        } catch (IOException | com.github.fge.jsonpatch.JsonPatchException e) {
            String message = "Failed to parse json patch content";
            log.trace(message, e);
            throw new JsonMergeException(message);
        }
    }

    public JsonParser createParser(InputStream inputStream) throws IOException {
        return this.createParser(instance.getMapper(), inputStream);
    }

    public JsonParser createParser(ObjectMapper objectMapper, InputStream inputStream) throws IOException {
        // 获取 JsonFactory 实例
        JsonFactory jsonFactory = objectMapper.getFactory();
        // 使用 JsonFactory 创建 JsonParser
        return jsonFactory.createParser(inputStream);
    }

    public <T> T readValue(JsonParser p, Class<T> valueType) throws IOException {
        return (T) instance.getMapper().readValue(p, valueType);
    }
}
