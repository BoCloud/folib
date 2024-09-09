package com.veadan.folib.domain;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.ObjectCodec;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import com.fasterxml.jackson.databind.JsonNode;
import java.io.IOException;
import java.time.LocalDateTime;
import java.time.Month;
import java.time.DayOfWeek;
import java.time.temporal.ChronoUnit;
import java.time.temporal.WeekFields;

public class LocalDateTimeDeserializer extends JsonDeserializer<LocalDateTime> {
    @Override
    public LocalDateTime deserialize(JsonParser jsonParser, DeserializationContext ctxt) throws IOException {
        ObjectCodec codec = jsonParser.getCodec();
        JsonNode node = codec.readTree(jsonParser);

        int year = node.get("year").asInt();
        int monthValue = node.get("monthValue").asInt();
        int dayOfMonth = node.get("dayOfMonth").asInt();
        int hour = node.get("hour").asInt();
        int minute = node.get("minute").asInt();
        int second = node.get("second").asInt();
        int nano = node.get("nano").asInt();

        return LocalDateTime.of(year, monthValue, dayOfMonth, hour, minute, second, nano);
    }
}
