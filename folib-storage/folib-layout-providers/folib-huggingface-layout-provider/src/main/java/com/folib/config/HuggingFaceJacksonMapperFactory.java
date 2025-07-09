/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.config;


import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.module.SimpleModule;
import org.apache.commons.lang3.StringUtils;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;

public class HuggingFaceJacksonMapperFactory {

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
