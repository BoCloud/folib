package com.folib.json;

import java.io.IOException;
import java.util.Map;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;

/**
 * @author veadan
 */
public class MapValuesJsonSerializer
        extends JsonSerializer<Map<?, ?>>
{


    @Override
    public void serialize(final Map<?, ?> value,
                          final JsonGenerator gen,
                          final SerializerProvider serializers)
            throws IOException
    {
        gen.writeObject(value.values());
    }
}
