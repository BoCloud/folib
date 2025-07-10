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
package com.folib.io;

import com.folib.commons.http.range.ByteRange;

import java.io.FilterInputStream;
import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.reflect.Field;

import org.springframework.util.ReflectionUtils;

public class StreamUtils
{

    private StreamUtils()
    {
    }

    public static long getLength(ByteRangeInputStream bris)
    {
        return bris.getLength();
    }

    public static void setCurrentByteRange(ByteRangeInputStream bris,
                                           ByteRange byteRange)
            throws IOException
    {
        if (bris != null)
        {
            bris.setCurrentByteRange(byteRange);
            bris.skip(byteRange.getOffset());
        }
    }

    public static <T extends InputStream> T findSource(Class<T> sourceClass,
                                                       InputStream in)
    {
        if (sourceClass.isAssignableFrom(in.getClass()))
        {
            return (T) in;
        }

        Field inField = ReflectionUtils.findField(FilterInputStream.class, "in");
        if (inField != null)
        {
            inField.setAccessible(true);

            InputStream source = in;
            while (source instanceof FilterInputStream)
            {
                try
                {
                    source = (InputStream) inField.get(source);
                }
                catch (Exception e)
                {
                    return null;
                }
                if (sourceClass.isAssignableFrom(source.getClass()))
                {
                    return sourceClass.cast(source);
                }
            }
        }
        return null;
    }

    public static <T extends OutputStream> T findSource(Class<T> sourceClass,
                                                        OutputStream out)
    {
        if (sourceClass.isAssignableFrom(out.getClass()))
        {
            return (T) out;
        }

        Field outField = ReflectionUtils.findField(FilterOutputStream.class, "out");
        if (outField != null)
        {
            outField.setAccessible(true);

            OutputStream source = out;
            while (source instanceof FilterOutputStream)
            {
                try
                {
                    source = (OutputStream) outField.get(source);
                }
                catch (Exception e)
                {
                    return null;
                }
                if (sourceClass.isAssignableFrom(source.getClass()))
                {
                    return sourceClass.cast(source);
                }
            }
        }
        return null;
    }

}
