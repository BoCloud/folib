package com.veadan.folib.domain.huggingface.common;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import com.fasterxml.jackson.databind.JsonNode;
import java.io.IOException;
import java.net.URI;
import java.net.URL;
import org.apache.commons.lang3.StringUtils;

public class SafeUriDeserializer extends JsonDeserializer<URI> {
    public URI deserialize(JsonParser jsonParser, DeserializationContext deserializationContext) throws IOException {
        JsonNode node = (JsonNode)jsonParser.getCodec().readTree(jsonParser);
        String value = node.asText();
        return create(value);
    }

    static URI create(String uri) {
        if (StringUtils.isBlank(uri))
            return URI.create("");
        try {
            URL url = new URL(uri);
            String path = url.getPath();
            String query = url.getQuery();
            String fragment = url.getRef();
            String protocol = url.getProtocol();
            String userInfo = url.getUserInfo();
            String host = url.getHost();
            int port = url.getPort();
            return new URI(protocol, userInfo, host, port, path, query, fragment);
        } catch (Exception e) {
            throw new IllegalArgumentException(e.getMessage(), e);
        }
    }
}
