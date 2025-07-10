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

import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;

/**
 * Allows to create {@link InputStream} instance with "lazy" initialization.
 * This mean that underlaing {@link InputStream} instance will be created only
 * when reading directly occures. <br>
 * For example this needed if resource should be locked before reading.
 * 
 * @author veadan
 */
public class LazyInputStream extends FilterInputStream
{

    private static final String ERROR_FAILED_TO_CREATE_INPUT_STREAM = "Failed to create InputStream.";

    private InputStreamSupplier supplier;

    public LazyInputStream(InputStreamSupplier supplier)
    {
        super(null);
        this.supplier = supplier;
    }

    @Override
    public int read()
        throws IOException
    {
        init();
        return in.read();
    }

    @Override
    public int read(byte[] b)
        throws IOException
    {
        init();
        return in.read(b);
    }

    @Override
    public int read(byte[] b,
                    int off,
                    int len)
        throws IOException
    {
        init();
        return in.read(b, off, len);
    }

    @Override
    public long skip(long n)
        throws IOException
    {
        init();
        return in.skip(n);
    }

    @Override
    public int available()
        throws IOException
    {
        init();
        return in.available();
    }

    @Override
    public void close()
        throws IOException
    {
        if (in == null)
        {
            return;
        }
        in.close();
    }

    @Override
    public synchronized void mark(int readlimit)
    {
        in.mark(readlimit);
    }

    @Override
    public synchronized void reset()
        throws IOException
    {
        init();
        in.reset();
    }

    @Override
    public boolean markSupported()
    {
        return in.markSupported();
    }

    public void init()
        throws IOException
    {
        if (in != null)
        {
            return;
        }

        try
        {
            in = supplier.get();
        }
        catch (IOException e)
        {
            throw e;
        }
        catch (Exception e)
        {
            throw new IOException(ERROR_FAILED_TO_CREATE_INPUT_STREAM, e);
        }
        finally
        {
            supplier = null;
        }
    }

    @FunctionalInterface
    public static interface InputStreamSupplier
    {

        InputStream get()
            throws IOException;

    }

}
