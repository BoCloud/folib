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
package com.folib.indexer;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringUtils;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.Arrays;

/**
 * @author veadan
 * @since 2024-09-03 14:41
 */
@Slf4j
public class IncrementalIndexStreamer {
    private static final String DELIMITER = "\n\n";
    private static final byte[] DELIMITER_BYTES = "\n\n".getBytes();
    private final InputStream in;
    private final DeltaBasedIndexFilter filter;
    private final File target;

    public IncrementalIndexStreamer(InputStream in, File target, DeltaBasedIndexFilter filter) {
        this.in = in;
        this.filter = filter;
        this.target = target;
    }


    public void write() throws IOException {
        log.debug("Writing temp packages file {}", this.target.getAbsolutePath());
        try (OutputStream out = Files.newOutputStream(this.target.toPath())) {
            if (this.filter.hasPendingRemovals()) {
                log.info("Filter has pending removals, streaming blocks.");
                this.streamBlocks(out);
            } else {
                log.debug("Filter has no pending removals, writing stream to file as is.");
                IOUtils.copyLarge(this.in, out);
            }
            this.appendBlocks(out);
            out.flush();
        }
    }

    public void streamBlocks(OutputStream out) throws IOException {
        EfficientBufferedReader buffer = new EfficientBufferedReader(this.in);
        StringBuilder builder = new StringBuilder();

        String line;
        while ((line = buffer.readLine()) != null) {
            builder.setLength(0);
            String filenameField = null;
            while (StringUtils.isNotBlank(line)) {
                String lineWithoutSpace = line.trim();
                if (filenameField == null && lineWithoutSpace.startsWith(this.filter.getLineIdentifier())) {
                    filenameField = lineWithoutSpace;
                }
                builder.append(line);
//                builder.append(line).append(System.lineSeparator());
                line = buffer.readLine();
            }
            if (filenameField != null && !this.filter.shouldRemoveBlock(filenameField)) {
                this.alignBlockAndWrite(out, builder.toString());
            }
        }
    }

    private void alignBlockAndWrite(OutputStream out, String block) throws IOException {
        if (!block.endsWith("\n\n")) {
            if (block.endsWith("\n")) {
                block = block + "\n";
            } else {
                block = block + "\n\n";
            }
        }

        out.write(block.getBytes());
    }

    private void appendBlocks(OutputStream out) throws IOException {
        boolean blocksWritten = false;
        while(this.filter.hasNextAddBlock()) {
            String nextBlock = this.getNextBlock();
            this.logNextBlock(nextBlock);
            out.write(nextBlock.getBytes());
            if (StringUtils.isNotBlank(nextBlock)) {
                blocksWritten = true;
            }
        }
        if (blocksWritten) {
            out.write(DELIMITER_BYTES);
        }

    }

    private String getNextBlock() {
        String nextBlock = this.filter.getNextAddBlock();
        log.trace("Got next block {} from filter", nextBlock);
        if (this.filter.hasNextAddBlock() && nextBlock.length() > 0) {
            nextBlock = nextBlock + "\n\n";
        }

        return nextBlock;
    }

    private void logNextBlock(String nextBlock) {
        if (log.isTraceEnabled()) {
            log.trace("Appending next block {} to file", nextBlock);
        }

    }
    private class EfficientBufferedReader {
        private byte[] fromStream = new byte[10240];
        private byte[] currentBlockBuffer = new byte[10240];
        private int streamPosition = 0;
        private int length = 0;
        private InputStream in;

        EfficientBufferedReader(InputStream in) {
            this.in = in;
        }

        String readLine() throws IOException {
            int position = 0;
            for(int available = this.tryToFillBuffer(); available != -1 || this.streamPosition < this.length; available = this.tryToFillBuffer()) {
                byte read = this.fromStream[this.streamPosition++];
                if (position >= this.currentBlockBuffer.length || position + 1 >= this.currentBlockBuffer.length) {
                    this.growBlockBuffer();
                }
                this.currentBlockBuffer[position++] = read;
                if (read == 10) {
                    return new String(this.currentBlockBuffer, 0, position);
                }
            }
            if (position > 0) {
                return new String(this.currentBlockBuffer, 0, position, StandardCharsets.UTF_8);
            } else {
                return null;
            }
        }

        private int tryToFillBuffer() throws IOException {
            if (this.streamPosition == this.length) {
                this.length = this.in.read(this.fromStream);
                if (this.length < 0) {
                    this.streamPosition = 0;
                    return -1;
                }

                this.streamPosition = 0;
            }
            return this.length - this.streamPosition;
        }

        private void growBlockBuffer() {
            int oldCapacity = this.currentBlockBuffer.length;
            int newCapacity = oldCapacity + (oldCapacity >> 1);
            this.currentBlockBuffer = Arrays.copyOf(this.currentBlockBuffer, newCapacity);
        }
    }


}
