package com.veadan.folib.model.seaializer;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.veadan.folib.util.HelmVersionUtil;
import java.io.IOException;
import java.util.Map;

public class ChartAnnotationsSerializer extends JsonSerializer<Map<String, JsonNode>> {
    public void serialize(Map<String, JsonNode> stringJsonNodeMap, JsonGenerator jsonGenerator, SerializerProvider serializerProvider) throws IOException {
        jsonGenerator.writeStartObject();
        for (Map.Entry<String, JsonNode> entry : stringJsonNodeMap.entrySet()) {
            if (((JsonNode)entry.getValue()).isTextual()) {
                try {
                    Float.parseFloat(((JsonNode)entry.getValue()).textValue());
                    jsonGenerator.writeObjectField(entry.getKey(),
                            HelmVersionUtil.markWithReplacePattern(((JsonNode)entry.getValue()).textValue()));
                } catch (NumberFormatException e) {
                    jsonGenerator.writeObjectField(entry.getKey(), ((JsonNode)entry.getValue()).textValue());
                }
                continue;
            }
            jsonGenerator.writeObjectField(entry.getKey(), entry.getValue());
        }
        jsonGenerator.writeEndObject();
    }
}