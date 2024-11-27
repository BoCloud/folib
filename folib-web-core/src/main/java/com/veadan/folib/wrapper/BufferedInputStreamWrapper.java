package com.veadan.folib.wrapper;

import java.io.BufferedInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.RandomAccessFile;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/12/16 17:33
 * @since x.x.x
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
