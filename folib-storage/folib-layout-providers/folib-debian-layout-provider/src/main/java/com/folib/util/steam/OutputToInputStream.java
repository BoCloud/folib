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

import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

/**
 * @author veadan
 * @since 2024-09-02 17:01
 */
@Slf4j
public abstract class OutputToInputStream extends InputStream {
    private final ExecutorService executor;
    private final int bufferSize;
    private PipedInputStream pipedInputStream;
    private Future writerFuture;
    private boolean resultChecked;

    public OutputToInputStream() {
        this(Executors.newSingleThreadExecutor());
    }

    public OutputToInputStream(ExecutorService executor) {
        this(executor, 4096);
    }

    public OutputToInputStream(ExecutorService executor, int bufferSize) {
        this.executor = executor;
        this.bufferSize = bufferSize;
    }

    protected abstract void write(OutputStream var1) throws IOException;

    public int read() throws IOException {
        if (this.pipedInputStream == null) {
            this.initializePipedStream();
        }

        int read = this.pipedInputStream.read();
        this.checkForException(read);
        return read;
    }

    public int read(byte[] b, int off, int len) throws IOException {
        if (this.pipedInputStream == null) {
            this.initializePipedStream();
        }

        int read = this.pipedInputStream.read(b, off, len);
        this.checkForException(read);
        return read;
    }

    public void close() throws IOException {
        if (this.pipedInputStream == null) {
            log.debug("Called close() and piped stream is null");
        } else {
            this.pipedInputStream.close();
            if (this.writerFuture == null) {
                log.debug("Called close() and result is null");
            } else if (!this.writerFuture.isDone()) {
                log.debug("Stream closed while writer still running");
            } else {
                this.checkForException(-1);
            }
        }
    }

    private void initializePipedStream() throws IOException {
        log.info("Initializing piped input stream");
        this.pipedInputStream = new PipedInputStream(this.bufferSize);
        final PipedOutputStream sink = new PipedOutputStream(this.pipedInputStream);
        Callable worker = (Callable<Void>) () -> {
            Object result;
            try {
                OutputToInputStream.this.write(sink);
                result = null;
            } finally {
                try {
                    sink.close();
                } catch (IOException var8) {
                    OutputToInputStream.log.debug("Failed to close piped output stream", var8);
                }
            }
            return (Void) result;
        };
        this.writerFuture = this.executor.submit(worker);
    }

    private void checkForException(int read) throws IOException {
        if ((read < 0 || this.writerFuture.isDone()) && !this.resultChecked) {
            try {
                this.resultChecked = true;
                this.writerFuture.get();
            } catch (InterruptedException interruptedException) {
                throw new IOException(interruptedException);
            } catch (ExecutionException runtimeException) {
                if (runtimeException.getCause() != null) {
                    throw new IOException(runtimeException.getCause());
                }
                throw new IOException(runtimeException);
            }
        }

    }
}
