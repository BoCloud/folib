package com.veadan.folib.cron.jobs.fields;

import java.io.IOException;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;

/**
 * @author veadan
 */
public class CronJobFieldJsonSerializer
        extends JsonSerializer<CronJobField>
{

    @Override
    public void serialize(CronJobField value,
                          JsonGenerator gen,
                          SerializerProvider serializers)
            throws IOException
    {
        gen.writeStartObject();
        CronJobField temp = value;
        while (temp != null)
        {
            gen.writeStringField(temp.getKey(), temp.getValue());
            temp = temp.getField();
        }
        gen.writeEndObject();
    }
}
