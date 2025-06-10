package com.veadan.folib.commons.io;

import com.veadan.folib.commons.http.range.ByteRange;
import com.veadan.folib.commons.io.reloading.ReloadableInputStreamHandler;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;

/**
 * @author carlspring
 */
public class ByteRangeInputStream
        extends AbstractByteRangeInputStream
{

    private long length;


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

    public ByteRangeInputStream(InputStream is)
    {
        super(is);
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
    public void reload()
            throws IOException
    {
        reloadableInputStreamHandler.reload();
        in = reloadableInputStreamHandler.getInputStream();
    }

    @Override
    public void reposition()
            throws IOException
    {
        if (byteRanges != null && !byteRanges.isEmpty() && currentByteRangeIndex < byteRanges.size())
        {
            ByteRange current = currentByteRange;

            currentByteRangeIndex++;
            currentByteRange = byteRanges.get(currentByteRangeIndex);

            if (currentByteRange.getOffset() > current.getLimit())
            {
                // If the offset is higher than the current position, skip forward
                long bytesToSkip = currentByteRange.getOffset() - current.getLimit();

                //noinspection ResultOfMethodCallIgnored
                in.skip(bytesToSkip);
            }
            else
            {
                reloadableInputStreamHandler.reload();
                in = reloadableInputStreamHandler.getInputStream();
            }
        }
    }

    @Override
    public void reposition(long skipBytes)
    {

    }

}
