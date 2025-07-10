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
package com.folib.providers.io;

import java.io.IOException;
import java.io.InputStream;

/**
 * @author veadan
 * @date 1/13/2024 12:04
 */
public abstract class AbsFallbackRemoteArtifactInputStream extends InputStream {

  private InputStream target;
    private  RepositoryPath artifactPath;

    protected abstract InputStream intiTarget()  throws IOException;
    private InputStream getTarget()
            throws IOException
    {
        if (target != null)
        {
            return target;
        }

//        target = getConnection().getResponse().readEntity(InputStream.class);
//        if (target == null)
//        {
//            throw new IOException(String.format("Unexpected null as InputStream response for %s.",
//                    resource));
//        }
        target = intiTarget();
        return target;
    }
    public int read()
            throws IOException
    {
        return getTarget().read();
    }

    public int read(byte[] b)
            throws IOException
    {
        return getTarget().read(b);
    }

    public int read(byte[] b,
                    int off,
                    int len)
            throws IOException
    {
        return getTarget().read(b, off, len);
    }

    public long skip(long n)
            throws IOException
    {
        return getTarget().skip(n);
    }

    public int available()
            throws IOException
    {
        return getTarget().available();
    }

    public void mark(int readlimit)
    {
        throw new UnsupportedOperationException();
    }

    public void reset()
            throws IOException
    {
        getTarget().reset();
    }

    public boolean markSupported()
    {
        return false;
    }

    @Override
    public void close()
            throws IOException
    {
        try
        {
            if (target != null)
            {
                target.close();
            }
        } finally
        {
           // closeConnection();
        }
    }

}
