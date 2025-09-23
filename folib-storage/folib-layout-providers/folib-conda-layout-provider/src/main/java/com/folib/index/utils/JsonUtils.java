package com.folib.index.utils;

import com.fasterxml.jackson.databind.ObjectMapper;

public class JsonUtils {
    private static final ObjectMapper objectMapper = new ObjectMapper();
    private static final JsonUtils INSTANCE = new JsonUtils();

    private JsonUtils() {
        // 私有构造函数，确保单例模式
    }

    public static JsonUtils getInstance() {
        return INSTANCE;
    }

    public <T> T readValue(String json, Class<T> clazz) throws Exception {
        if (json == null || clazz == null) {
            throw new IllegalArgumentException("JSON string or target class cannot be null");
        }
        return objectMapper.readValue(json, clazz);
    }
}
