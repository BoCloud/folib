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
package com.folib.jtwig.extensions;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;

import org.jtwig.functions.FunctionRequest;
import org.jtwig.functions.JtwigFunction;

/**
 * 
 * @author veadan
 *
 */
public class HumanReadableSizeConversionFunction implements JtwigFunction
{

    @Override
    public String name()
    {
        return "humanReadableSize";
    }

    @Override
    public Collection<String> aliases()
    {
        return Collections.unmodifiableList(Arrays.asList("readableSize"));
    }

    @Override
    public Object execute(FunctionRequest request)
    {
        request.minimumNumberOfArguments(1);
        request.maximumNumberOfArguments(1);
        String byteSize = request.getEnvironment().getValueEnvironment().getStringConverter().convert(request.get(0));
        try
        {
            return getHumanReadableSize(Long.parseLong(byteSize));
        }
        catch (NumberFormatException nfe)
        {
            // Need to return same value of bytes
        }
        return byteSize;
    }

    private String getHumanReadableSize(long bytes)
    {
        int unit = 1000;
        if (bytes < unit)
        {
            return bytes + " B";
        }
        int exp = (int) (Math.log(bytes) / Math.log(unit));
        String pre = "" + "KMGTPE".charAt(exp - 1);
        return String.format("%.1f %sB", bytes / Math.pow(unit, exp), pre);
    }

}
