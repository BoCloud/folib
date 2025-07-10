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

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Deque;
import java.util.Iterator;
import java.util.LinkedList;

/**
 * This {@link InputStream} decorates a source {@link InputStream} with ability to replace a target chain of bytes with
 * another specified chain of bytes.<br>
 * 
 * For example it can be used as Mulitpart Stream Adapter to change boundary bytes.
 * 
 * @author @author veadan
 * 
 */
public class ReplacingInputStream
        extends BufferedInputStream
{

    Deque<Integer> inQueue = new LinkedList<Integer>();

    Deque<Integer> outQueue = new LinkedList<Integer>();

    final byte[] search, replacement;

    public ReplacingInputStream(InputStream in,
                                byte[] search,
                                byte[] replacement)
    {
        super(in);
        this.search = search;
        this.replacement = replacement;
    }

    private boolean isMatchFound()
    {
        Iterator<Integer> inIter = inQueue.iterator();
        for (int i = 0; i < search.length; i++)
        {
            if (!inIter.hasNext() || search[i] != inIter.next())
            {
                return false;
            }
        }
        return true;
    }

    private void readAhead()
        throws IOException
    {
        // Work up some look-ahead.
        while (inQueue.size() < search.length)
        {
            int next = super.read();
            inQueue.offer(next);
            if (next == -1)
            {
                break;
            }
        }
    }

    @Override
    public int read()
        throws IOException
    {

        // Next byte already determined.
        if (outQueue.isEmpty())
        {

            readAhead();

            if (isMatchFound())
            {
                for (int i = 0; i < search.length; i++)
                {
                    inQueue.remove();
                }

                for (byte b : replacement)
                {
                    outQueue.offer((int) b);
                }
            }
            else
            {
                outQueue.add(inQueue.remove());
            }
        }

        return outQueue.remove();
    }

    /**
     * Returns false. REFilterInputStream does not support mark() and reset() methods.
     */
    @Override
    public boolean markSupported()
    {
        return false;
    }

    /**
     * Reads from the stream into the provided array.
     */
    @Override
    public int read(byte[] b,
                    int off,
                    int len)
        throws IOException
    {
        int i;
        int ok = 0;
        while (len-- > 0)
        {
            i = read();
            if (i == -1)
            {
                return (ok == 0) ? -1 : ok;
            }
            b[off++] = (byte) i;
            ok++;
        }
        return ok;
    }

    @Override
    public int read(byte[] buffer)
        throws IOException
    {

        return read(buffer, 0, buffer.length);
    }
}
