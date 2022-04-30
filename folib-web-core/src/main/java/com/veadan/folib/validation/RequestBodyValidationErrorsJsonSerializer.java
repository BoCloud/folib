package com.veadan.folib.validation;


import java.io.IOException;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;
import org.springframework.util.MultiValueMap;

/**
 * @author Przemyslaw Fusik
 */
public class RequestBodyValidationErrorsJsonSerializer
        extends JsonSerializer<MultiValueMap<String, String>>
{

    @Override
    public void serialize(final MultiValueMap<String, String> value,
                          final JsonGenerator gen,
                          final SerializerProvider serializers)
            throws IOException
    {
        gen.writeStartArray();
        for (final Map.Entry<String, List<String>> entry : value.entrySet())
        {
            gen.writeStartObject();
            gen.writeFieldName("name");
            gen.writeString( entry.getKey());
            gen.writeFieldName("messages");
            gen.writeStartArray();
            for (final String entryValue : entry.getValue())
            {
                gen.writeString(entryValue);
            }
            gen.writeEndArray();
            gen.writeEndObject();
        }
        gen.writeEndArray();
    }
}
