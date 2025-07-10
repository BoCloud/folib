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
import com.folib.commons.io.reloading.Reloading;
import com.folib.commons.io.reloading.Repositioning;

import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * @author carlspring
 */
public abstract class AbstractByteRangeInputStream
        extends FilterInputStream
        implements Reloading,
                   Repositioning,
        ResourceWithLength
{
    private boolean rangedMode = false;

    /**
     * The number of bytes read from the stream, or from this byte range.
     */
    protected long bytesRead = 0L;
    /**
     * The number of bytes to read from the start of the stream, before stopping to read.
     */
    protected long limit = 0L;

    protected List<ByteRange> byteRanges = new ArrayList<>();

    protected ByteRange currentByteRange;

    protected int currentByteRangeIndex = 0;

    protected ReloadableInputStreamHandler reloadableInputStreamHandler;

    public AbstractByteRangeInputStream(ReloadableInputStreamHandler handler,
                                        ByteRange byteRange)
            throws IOException
    {
        super(handler.getInputStream());

        this.reloadableInputStreamHandler = handler;
        this.byteRanges = Collections.singletonList(byteRange);
        this.currentByteRange = byteRange;
        this.rangedMode = true;
    }

    public AbstractByteRangeInputStream(ReloadableInputStreamHandler handler,
                                        List<ByteRange> byteRanges)
            throws IOException
    {
        super(handler.getInputStream());
        this.reloadableInputStreamHandler = handler;
        this.byteRanges = byteRanges;
        this.currentByteRange = byteRanges.get(0);
        this.rangedMode = true;
    }

    public AbstractByteRangeInputStream(InputStream is)
    {
        super(is);
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
    public boolean hasMoreByteRanges()
    {
        return currentByteRangeIndex < byteRanges.size();
    }

    public boolean hasReachedLimit()
    {
        return limit > 0 && bytesRead >= limit;
    }

    public long getLimit()
    {
        return limit;
    }

    public void setLimit(long limit)
    {
        this.limit = limit;
    }

    public long getBytesRead()
    {
        return bytesRead;
    }

    public void setBytesRead(long bytesRead)
    {
        this.bytesRead = bytesRead;
    }

    public ReloadableInputStreamHandler getReloadableInputStreamHandler()
    {
        return this.reloadableInputStreamHandler;
    }

    public void setReloadableInputStreamHandler(ReloadableInputStreamHandler reloadableInputStreamHandler)
    {
        this.reloadableInputStreamHandler = reloadableInputStreamHandler;
    }

    public List<ByteRange> getByteRanges()
    {
        return byteRanges;
    }

    public void setByteRanges(List<ByteRange> byteRanges)
    {
        this.byteRanges = byteRanges;
    }

    public ByteRange getCurrentByteRange()
    {
        return currentByteRange;
    }

    public void setCurrentByteRange(ByteRange currentByteRange)
    {
        this.currentByteRange = currentByteRange;
    }

    public boolean isRangedMode()
    {
        return rangedMode;
    }

    public void setRangedMode(boolean rangedMode)
    {
        this.rangedMode = rangedMode;
    }

}
