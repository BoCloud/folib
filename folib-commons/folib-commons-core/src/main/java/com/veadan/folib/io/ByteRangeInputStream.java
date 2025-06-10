package com.veadan.folib.io;

import com.veadan.folib.commons.http.range.ByteRange;
import com.veadan.folib.commons.io.AbstractByteRangeInputStream;
import com.veadan.folib.commons.io.reloading.ReloadableInputStreamHandler;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;

public class ByteRangeInputStream
        extends AbstractByteRangeInputStream
{

    private long length;

    public ByteRangeInputStream(InputStream is)
    {
        super(is);
    }

    public ByteRangeInputStream(ReloadableInputStreamHandler handler,
                                ByteRange byteRange)
            throws IOException
    {
        super(handler, byteRange);
    }

    public ByteRangeInputStream(ReloadableInputStreamHandler handler,
                                List<ByteRange> byteRanges)
            throws IOException
    {
        super(handler, byteRanges);
    }

    @Override
    public void reposition(long skipBytes)
    {
        //Do noting here
    }

    @Override
    public long getLength()
    {
        return length;
    }

    public void setLength(long length)
    {
        this.length = length;
    }

    @Override
    public int read(byte[] bytes,
                    int off,
                    int len)
            throws IOException
    {
        if (hasReachedLimit())
        {
            return -1;
        }

        int numberOfBytesRead = in.read(bytes, off, len);
        if (limit > 0 && bytesRead < limit)
        {
            bytesRead += numberOfBytesRead;
        }

        return numberOfBytesRead;
    }

    @Override
    public int read(byte[] bytes)
            throws IOException
    {
        if (hasReachedLimit())
        {
            return -1;
        }

        int len = in.read(bytes);

        bytesRead += len;

        if (limit > 0 && bytesRead < limit)
        {
            bytesRead += len;
        }

        return len;
    }

}
