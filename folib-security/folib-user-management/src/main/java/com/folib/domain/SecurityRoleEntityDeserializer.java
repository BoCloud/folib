package com.folib.domain;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.ObjectCodec;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import com.fasterxml.jackson.databind.JsonNode;

import java.io.IOException;

public class SecurityRoleEntityDeserializer extends JsonDeserializer<SecurityRoleEntity> {
    @Override
    public SecurityRoleEntity deserialize(JsonParser jsonParser, DeserializationContext context) throws IOException {
        ObjectCodec codec = jsonParser.getCodec();
        JsonNode node = codec.readTree(jsonParser);

        SecurityRoleEntity role = new SecurityRoleEntity();
        role.setNativeId(Long.valueOf(node.get("nativeId").asText()));
        role.setUuid(node.get("uuid").asText());
        
        // You can optionally log or handle the roleName if needed
        if (node.has("roleName")) {
            String roleName = node.get("roleName").asText();
            // Handle the roleName if necessary
        }

        return role;
    }
}
