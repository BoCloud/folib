package com.folib.config;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.module.SimpleModule;
import org.apache.commons.lang3.StringUtils;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;

public class GitLfsJacksonMapperFactory {

    private static final String FORMAT1 = "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'";

    private static final String FORMAT2 = "yyyy-MM-dd'T'HH:mm:ss'Z'";

    public static ObjectMapper createObjectMapper() {
        ObjectMapper objectMapper = new ObjectMapper().enable(DeserializationFeature.ACCEPT_SINGLE_VALUE_AS_ARRAY)
                .enable(DeserializationFeature.UNWRAP_SINGLE_VALUE_ARRAYS);

        DateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'");
        objectMapper.setDateFormat(df);
        SimpleModule module = new SimpleModule();



        objectMapper.registerModule(module);

        SimpleModule timeModule = new SimpleModule();
        String dateFormats = System.getProperty("NPM_DATE_FORMAT");
        String[] dateFormatArr = new String[]{FORMAT1, FORMAT2};
        if (StringUtils.isNotBlank(dateFormats)) {
            dateFormatArr = dateFormats.split(",");
        }
        System.out.println(String.format("NPM_DATE_FORMAT %s", Arrays.toString(dateFormatArr)));
        timeModule.addDeserializer(Date.class, new CustomDateDeserializer(dateFormatArr));
        objectMapper.registerModule(timeModule);

        return objectMapper;
    }

}
