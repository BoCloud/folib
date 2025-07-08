package com.folib.gremlin.adapters;

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;

import org.neo4j.ogm.typeconversion.AttributeConverter;

/**
 * @author veadan
 */
public class DateConverter implements AttributeConverter<LocalDateTime, Long>
{

    @Override
    public Long toGraphProperty(LocalDateTime value)
    {
        return value.atZone(ZoneId.systemDefault()).toEpochSecond();
    }

    @Override
    public LocalDateTime toEntityAttribute(Long value)
    {
        return LocalDateTime.ofInstant(Instant.ofEpochMilli(value), ZoneId.systemDefault());
    }
}
