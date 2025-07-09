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
package com.folib.ext.jersey;

import javax.ws.rs.core.MediaType;
import java.text.ParseException;
import java.util.Map;
import java.util.regex.Pattern;

import org.glassfish.jersey.internal.LocalizationMessages;
import org.glassfish.jersey.message.internal.HttpHeaderReader;
import org.glassfish.jersey.message.internal.MediaTypeProvider;
import org.springframework.util.Assert;

/**
 * @author veadan
 */
public class CustomJerseyMediaTypeProvider
        extends MediaTypeProvider
{

    /**
     * Remote repository maven.oracle.com responses with incomplete content-type header like 'Application/jar; charset=' or 'Application/pom; charset='
     */
    private static final Pattern INCOMPLETE_CONTENT_TYPE_HEADER_PATTERN = Pattern.compile(
            "^Application/[a-z/+/-]+; charset=$");

    private static MediaType valueOf(String header)
            throws ParseException
    {
        HttpHeaderReader reader = HttpHeaderReader.newInstance(header);

        // Skip any white space
        reader.hasNext();

        // Get the type
        final String type = reader.nextToken().toString();
        reader.nextSeparator('/');
        // Get the subtype
        final String subType = reader.nextToken().toString();

        Map<String, String> params = null;

        if (reader.hasNext())
        {
            try
            {
                params = HttpHeaderReader.readParameters(reader);
            }
            catch (ParseException ex)
            {
                if (!(LocalizationMessages.HTTP_HEADER_END_OF_HEADER().equals(ex.getMessage()) &&
                      INCOMPLETE_CONTENT_TYPE_HEADER_PATTERN.matcher(header).matches()))
                {
                    throw ex;
                }
            }

        }

        return new MediaType(type, subType, params);
    }

    @Override
    public MediaType fromString(String header)
    {

        Assert.notNull(header, "header should not be null");

        try
        {
            return valueOf(header);
        }
        catch (ParseException ex)
        {
            throw new IllegalArgumentException("Error parsing media type '" + header + "'", ex);
        }
    }
}
