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
