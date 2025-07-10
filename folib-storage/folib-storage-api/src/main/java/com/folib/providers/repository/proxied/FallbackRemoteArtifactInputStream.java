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
package com.folib.providers.repository.proxied;

import com.folib.artifact.ArtifactNotFoundException;

import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.function.Function;

/**
 * @author veadan
 * @date 1/12/2024 20:36
 * <p>
 * Non Thread safe
 */
public class FallbackRemoteArtifactInputStream extends FilterInputStream {

    private final Function<Exception, InputStream> fallbackInputStreamSupplier;
    private boolean usingFallback = false;

    protected FallbackRemoteArtifactInputStream(InputStream primaryInputStream
            , Function<Exception, InputStream> fallbackInputStreamSupplier) {
        super(primaryInputStream);
        this.fallbackInputStreamSupplier = fallbackInputStreamSupplier;
    }
    private void switchToFallback(Exception e) throws ArtifactNotFoundException {
        if (usingFallback) {
            if (e instanceof ArtifactNotFoundException){
                throw (ArtifactNotFoundException)e;
            }
            throw new RuntimeException("Fallback Exception", e);
        }
        usingFallback = true;
        try {
            super.close();
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }
        InputStream apply = fallbackInputStreamSupplier.apply(e);
        if (apply == null) {
            throw new RuntimeException(e);
        }
        super.in = apply;

    }

    @Override
    public int read() throws IOException {
        try {
            return super.read();
        } catch (Exception e) {
            switchToFallback(e);
            return read();
        }
    }

    @Override
    public int read( byte[] b) throws IOException {
        try {
            return super.read(b);
        } catch (Exception e) {
            switchToFallback(e);
            return read(b);
        }

    }

    @Override
    public int read( byte[] b, int off, int len) throws IOException {
        try {
            return super.read(b, off, len);
        } catch (Exception e) {
            switchToFallback(e);
            return read(b, off, len);
        }
    }

    @Override
    public long skip(long n) throws IOException {
        try {
            return super.skip(n);
        } catch (Exception e) {
            switchToFallback(e);
            return skip(n);
        }
    }

    @Override
    public void close() throws IOException {
        super.close();
    }

    @Override
    public void mark(int readlimit) {
        try {
            super.mark(readlimit);
        } catch (Exception e) {
            try {
                switchToFallback(e);
            } catch (ArtifactNotFoundException ex) {
                throw new RuntimeException(ex);
            }
            mark(readlimit);
        }
    }

    @Override
    public void reset() throws IOException {
        try {
            super.reset();
        } catch (Exception e) {
            switchToFallback(e);
            reset();
        }
    }

    @Override
    public boolean markSupported() {
        try {
            return super.markSupported();
        } catch (Exception e) {
            try {
                switchToFallback(e);
            } catch (ArtifactNotFoundException ex) {
                throw new RuntimeException(ex);
            }
            return markSupported();
        }
    }

    @Override
    public int available() throws IOException {
        try {
            return super.available();
        } catch (Exception e) {
            switchToFallback(e);
            return available();
        }
    }
}
