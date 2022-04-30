package com.veadan.folib.storage;

import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryData;

import java.io.IOException;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.Map;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.ObjectCodec;
import com.fasterxml.jackson.core.TreeNode;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * @author Przemyslaw Fusik
 */
public class RepositoryArrayToMapJsonDeserializer
        extends JsonDeserializer<Map<String, Repository>>
{

    @Override
    public Map<String, Repository> deserialize(final JsonParser parser,
                                               final DeserializationContext ctxt)
            throws IOException
    {
        Map<String, Repository> result = new LinkedHashMap<>();

        ObjectCodec codec = parser.getCodec();
        TreeNode node = codec.readTree(parser);
        if (node.isArray() && node.size() > 0)
        {
            Repository[] repositories = ((ObjectMapper) codec).readValue(node.toString(), RepositoryData[].class);
            Arrays.stream(repositories).forEach(r -> result.put(r.getId(), r));
        }

        return result;
    }
}
