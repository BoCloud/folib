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
package com.folib.util.steam;

import org.apache.commons.compress.archivers.ArchiveEntry;
import org.apache.commons.compress.archivers.ArchiveInputStream;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.Arrays;
import java.util.zip.GZIPInputStream;

/**
 * @author veadan
 * @since 2024-09-03 10:15
 */
public class SmarchiveInputStream extends ArchiveInputStream {
    private InputStream is;
    private ArchiveInputStream actual;

    private SmarchiveInputStream(InputStream is) {
        this.is = is;
    }

    public static ArchiveInputStream realize(InputStream is) throws InvocationTargetException, NoSuchMethodException, InstantiationException, IllegalAccessException, IOException {
        SmarchiveInputStream smarchiveInputStream = new SmarchiveInputStream(is);
        smarchiveInputStream.realize();
        return smarchiveInputStream;
    }

    private void realize() throws IOException, InvocationTargetException, NoSuchMethodException, InstantiationException, IllegalAccessException {
        BufferedInputStream originalStream = new BufferedInputStream(this.is);
        originalStream = this.addGzFilterIfNeeded(originalStream);
        this.actual = this.wrapStreamWithSpecificArchiveImplementation(originalStream);
    }

    public int read() throws IOException {
        return this.actual.read();
    }

    public long getBytesRead() {
        return this.actual.getBytesRead();
    }

    public boolean canReadEntryData(ArchiveEntry archiveEntry) {
        return this.actual.canReadEntryData(archiveEntry);
    }

    public ArchiveEntry getNextEntry() throws IOException {
        return this.actual.getNextEntry();
    }

    public synchronized void reset() throws IOException {
        this.actual.reset();
    }

    public boolean markSupported() {
        return this.actual.markSupported();
    }

    public synchronized void mark(int readlimit) {
        this.actual.mark(readlimit);
    }

    public void close() throws IOException {
        this.actual.close();
    }

    public int available() throws IOException {
        return this.actual.available();
    }

    public int read(byte[] b) throws IOException {
        return this.actual.read(b);
    }

    public int read(byte[] b, int off, int len) throws IOException {
        return this.actual.read(b, off, len);
    }

    public long skip(long n) throws IOException {
        return this.actual.skip(n);
    }

    private BufferedInputStream addGzFilterIfNeeded(BufferedInputStream buffered) throws IOException {
        return this.streamBeginsWithHeader(new byte[]{31, -117}, 0, buffered) ? new BufferedInputStream(new GZIPInputStream(buffered)) : buffered;
    }

    private ArchiveInputStream wrapStreamWithSpecificArchiveImplementation(BufferedInputStream buffered) throws IOException, IllegalAccessException, InvocationTargetException, InstantiationException, NoSuchMethodException {
        MagicHeader[] headers = MagicHeader.values();
        for (int i = 0; i < headers.length; i++) {
            MagicHeader magicHeader = headers[i];
            if (this.streamBeginsWithHeader(magicHeader, buffered)) {
                return this.constructActualArchiveStream(magicHeader, buffered);
            }
        }
        throw new IllegalArgumentException("Given input stream could not be recognized as an archive stream");
    }

    private ArchiveInputStream constructActualArchiveStream(MagicHeader magicHeader, BufferedInputStream buffered) throws NoSuchMethodException, InstantiationException, IllegalAccessException, InvocationTargetException {
        Class<? extends ArchiveInputStream> archiveType = magicHeader.getArchiveType();
        Constructor<? extends ArchiveInputStream> constructor = archiveType.getConstructor(InputStream.class);
        return (ArchiveInputStream) constructor.newInstance(buffered);
    }

    private boolean streamBeginsWithHeader(MagicHeader magicHeader, BufferedInputStream buffered) throws IOException {
        byte[] headerValue = magicHeader.getHeader();
        return this.streamBeginsWithHeader(headerValue, magicHeader.getOffset(), buffered);
    }

    private boolean streamBeginsWithHeader(byte[] headerValue, int offset, BufferedInputStream buffered) throws IOException {
        buffered.mark(offset + headerValue.length + 1);
        byte[] streamHeader = new byte[headerValue.length];
        buffered.skip((long) offset);
        int streamBytesRead = buffered.read(streamHeader);
        if (streamBytesRead == 0) {
            throw new IOException("Unable to read bytes for magic header detection");
        } else {
            buffered.reset();
            return Arrays.equals(headerValue, streamHeader);
        }
    }
}
