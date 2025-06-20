package com.veadan.folib.utils;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.TreeNode;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.Module;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.github.fge.jsonpatch.mergepatch.JsonMergePatch;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.net.URL;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Consumer;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import com.veadan.folib.exception.JsonMergeException;
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
