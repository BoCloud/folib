package com.folib.nugetv3.model.deserializers;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.deser.std.StdDeserializer;
import com.fasterxml.jackson.databind.node.ArrayNode;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;


@Slf4j
public class AuthorsDeserializer extends StdDeserializer<String> {

    public AuthorsDeserializer() {
        super(String.class);
    }

    public String deserialize(JsonParser jsonParser, DeserializationContext deserializationContext) throws IOException {
        JsonNode authorsNode = (JsonNode) jsonParser.readValueAsTree();
        if (authorsNode.isObject()) {
            log.warn("Authors could not deserialize since the provided format was invalid, expected: ArrayNode/TextNode got ObjectNode");
            return null;
        } else {
            return authorsNode.isArray() ? this.formatArrayNode((ArrayNode) authorsNode) : authorsNode.asText();
        }
    }

    private String formatArrayNode(ArrayNode authors) {
        List<String> authorsList = new ArrayList();
        Iterator<JsonNode> keywordsIter = authors.elements();

        while (keywordsIter.hasNext()) {
            String author = ((JsonNode) keywordsIter.next()).asText();
            if (StringUtils.isNotBlank(author)) {
                authorsList.add(author);
            }
        }

        return String.join(", ", authorsList);
    }
}
