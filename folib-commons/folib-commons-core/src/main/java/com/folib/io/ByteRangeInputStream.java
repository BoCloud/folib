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
import com.folib.commons.io.AbstractByteRangeInputStream;
import com.folib.commons.io.reloading.ReloadableInputStreamHandler;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;

public class ByteRangeInputStream
        extends AbstractByteRangeInputStream
{

    private long length;

    public ByteRangeInputStream(InputStream is)
    {
        super(is);
    }

    public ByteRangeInputStream(ReloadableInputStreamHandler handler,
                                ByteRange byteRange)
            throws IOException
    {
        super(handler, byteRange);
    }

    public ByteRangeInputStream(ReloadableInputStreamHandler handler,
                                List<ByteRange> byteRanges)
            throws IOException
    {
        super(handler, byteRanges);
    }

    @Override
    public void reposition(long skipBytes)
    {
        //Do noting here
    }

    @Override
    public long getLength()
    {
        return length;
    }

    public void setLength(long length)
    {
        this.length = length;
    }

    @Override
    public int read(byte[] bytes,
                    int off,
                    int len)
            throws IOException
    {
        if (hasReachedLimit())
        {
            return -1;
        }

        int numberOfBytesRead = in.read(bytes, off, len);
        if (limit > 0 && bytesRead < limit)
        {
            bytesRead += numberOfBytesRead;
        }

        return numberOfBytesRead;
    }

    @Override
    public int read(byte[] bytes)
            throws IOException
    {
        if (hasReachedLimit())
        {
            return -1;
        }

        int len = in.read(bytes);

        bytesRead += len;

        if (limit > 0 && bytesRead < limit)
        {
            bytesRead += len;
        }

        return len;
    }

}
