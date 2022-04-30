package com.veadan.folib.json;

import java.io.IOException;
import java.util.LinkedHashMap;
import java.util.Map;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.ObjectCodec;
import com.fasterxml.jackson.core.TreeNode;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import com.fasterxml.jackson.databind.node.ArrayNode;

/**
 * @author Przemyslaw Fusik
 */
public class StringArrayToMapJsonDeserializer
        extends JsonDeserializer<Map<String, String>>
{

    @Override
    public Map<String, String> deserialize(final JsonParser parser,
                                           final DeserializationContext ctxt)
            throws IOException
    {
        Map<String, String> result = new LinkedHashMap<>();

        ObjectCodec codec = parser.getCodec();
        TreeNode node = codec.readTree(parser);

        if (node.isArray() && node.size() > 0)
        {
            ((ArrayNode) node).forEach(n -> result.put(n.asText(), n.asText()));
        }
        return result;
    }
}
