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
package com.folib.commons.io;

import com.folib.commons.http.range.ByteRange;
import com.folib.commons.io.reloading.ReloadableInputStreamHandler;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;

/**
 * @author carlspring
 */
public class ByteRangeInputStream
        extends AbstractByteRangeInputStream
{

    private long length;


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

    public ByteRangeInputStream(InputStream is)
    {
        super(is);
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
    public void reload()
            throws IOException
    {
        reloadableInputStreamHandler.reload();
        in = reloadableInputStreamHandler.getInputStream();
    }

    @Override
    public void reposition()
            throws IOException
    {
        if (byteRanges != null && !byteRanges.isEmpty() && currentByteRangeIndex < byteRanges.size())
        {
            ByteRange current = currentByteRange;

            currentByteRangeIndex++;
            currentByteRange = byteRanges.get(currentByteRangeIndex);

            if (currentByteRange.getOffset() > current.getLimit())
            {
                // If the offset is higher than the current position, skip forward
                long bytesToSkip = currentByteRange.getOffset() - current.getLimit();

                //noinspection ResultOfMethodCallIgnored
                in.skip(bytesToSkip);
            }
            else
            {
                reloadableInputStreamHandler.reload();
                in = reloadableInputStreamHandler.getInputStream();
            }
        }
    }

    @Override
    public void reposition(long skipBytes)
    {

    }

}
