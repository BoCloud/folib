package com.folib.controllers.support;

import java.io.IOException;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;

/**
 * @author Steve Todorov
 */
public class ListEntityBodyJsonSerializer
        extends JsonSerializer<ListEntityBody>
{

    @Override
    public void serialize(final ListEntityBody listEntityBody,
                          final JsonGenerator gen,
                          final SerializerProvider serializers)
            throws IOException
    {
        gen.writeStartObject();
        gen.writeFieldName(listEntityBody.getFieldName());
        gen.writeObject(listEntityBody.getList());
        gen.writeEndObject();
    }
}
