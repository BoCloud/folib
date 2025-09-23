package com.folib.nugetv3.utils;

import com.fasterxml.jackson.databind.ObjectMapper;


public class JsonUtil {
    private static final ObjectMapper objectMapper = new ObjectMapper();
    private static final JsonUtil INSTANCE = new JsonUtil();

    private JsonUtil() {
        // 私有构造函数，确保单例模式
    }

    public static JsonUtil getInstance() {
        return INSTANCE;
    }

    public <T> T readValue(String json, Class<T> clazz) throws Exception {
        if (json == null || clazz == null) {
            throw new IllegalArgumentException("JSON string or target class cannot be null");
        }
        return objectMapper.readValue(json, clazz);
    }
}
