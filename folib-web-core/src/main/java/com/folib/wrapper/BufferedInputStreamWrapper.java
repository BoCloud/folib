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
package com.folib.wrapper;

import java.io.BufferedInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.RandomAccessFile;

/**
 * @author veadan
 * @date 2023/12/16 17:33
 */
public class BufferedInputStreamWrapper extends InputStream {
    private RandomAccessFile randomAccessFile;
    private BufferedInputStream bufferedInputStream;
    private long currentPosition;
    private long endPosition;

    public BufferedInputStreamWrapper(String filePath, long startPosition, long numberOfBytesToRead) throws IOException {
        this.randomAccessFile = new RandomAccessFile(filePath, "r");
        this.currentPosition = startPosition;
        this.endPosition = startPosition + numberOfBytesToRead;
        this.randomAccessFile.seek(startPosition);

        final FileInputStream fileInputStream = new FileInputStream(randomAccessFile.getFD());
        this.bufferedInputStream = new BufferedInputStream(fileInputStream);
    }
    
    public BufferedInputStreamWrapper(InputStream inputStream, long startPosition, long numberOfBytesToRead) throws IOException {
        this.currentPosition = startPosition;
        this.endPosition = startPosition + numberOfBytesToRead;
        inputStream.skip(startPosition);
        
        this.bufferedInputStream = new BufferedInputStream(inputStream);
    }

    @Override
    public int read() throws IOException {
        if (currentPosition < endPosition) {
            int byteRead = bufferedInputStream.read();
            currentPosition++;
            return byteRead;
        } else {
            return -1;
        }
    }

    @Override
    public void close() throws IOException {
        bufferedInputStream.close();
        if (null != randomAccessFile) {
            randomAccessFile.close();
        }
    }
}
