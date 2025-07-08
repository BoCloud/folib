package com.folib.config;


import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;

import java.io.IOException;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Date;

public class CustomDateDeserializer extends JsonDeserializer<Date> {

    private DateTimeFormatter[] dateFormatters;

    public CustomDateDeserializer(String... dateFormats) {
        this.dateFormatters = new DateTimeFormatter[dateFormats.length];
        for (int i = 0; i < dateFormats.length; i++) {
            this.dateFormatters[i] = DateTimeFormatter.ofPattern(dateFormats[i]);
        }
    }

    @Override
    public Date deserialize(JsonParser p, DeserializationContext deserializationContext) throws IOException {
        String dateStr = p.getText();
        for (DateTimeFormatter formatter : dateFormatters) {
            try {
                LocalDateTime dateTime = LocalDateTime.parse(dateStr, formatter);
                return Date.from(dateTime.atZone(ZoneId.systemDefault()).toInstant());
            } catch (Exception e) {
                // Try the next format
                System.out.println(String.format("Parse error date [%s] format [%s]", dateStr, formatter.toString()));
            }
        }
        throw new IOException("Unparseable date: \"" + dateStr + "\"");
    }
}
