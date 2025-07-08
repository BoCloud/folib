package com.folib.commons.io;

import com.folib.commons.http.range.ByteRange;
import com.folib.commons.io.reloading.ReloadableInputStreamHandler;
import com.folib.commons.io.reloading.Reloading;
import com.folib.commons.io.reloading.Repositioning;

import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * @author carlspring
 */
public abstract class AbstractByteRangeInputStream
        extends FilterInputStream
        implements Reloading,
                   Repositioning,
        ResourceWithLength
{
    private boolean rangedMode = false;

    /**
     * The number of bytes read from the stream, or from this byte range.
     */
    protected long bytesRead = 0L;
    /**
     * The number of bytes to read from the start of the stream, before stopping to read.
     */
    protected long limit = 0L;

    protected List<ByteRange> byteRanges = new ArrayList<>();

    protected ByteRange currentByteRange;

    protected int currentByteRangeIndex = 0;

    protected ReloadableInputStreamHandler reloadableInputStreamHandler;

    public AbstractByteRangeInputStream(ReloadableInputStreamHandler handler,
                                        ByteRange byteRange)
            throws IOException
    {
        super(handler.getInputStream());

        this.reloadableInputStreamHandler = handler;
        this.byteRanges = Collections.singletonList(byteRange);
        this.currentByteRange = byteRange;
        this.rangedMode = true;
    }

    public AbstractByteRangeInputStream(ReloadableInputStreamHandler handler,
                                        List<ByteRange> byteRanges)
            throws IOException
    {
        super(handler.getInputStream());
        this.reloadableInputStreamHandler = handler;
        this.byteRanges = byteRanges;
        this.currentByteRange = byteRanges.get(0);
        this.rangedMode = true;
    }

    public AbstractByteRangeInputStream(InputStream is)
    {
        super(is);
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
    public boolean hasMoreByteRanges()
    {
        return currentByteRangeIndex < byteRanges.size();
    }

    public boolean hasReachedLimit()
    {
        return limit > 0 && bytesRead >= limit;
    }

    public long getLimit()
    {
        return limit;
    }

    public void setLimit(long limit)
    {
        this.limit = limit;
    }

    public long getBytesRead()
    {
        return bytesRead;
    }

    public void setBytesRead(long bytesRead)
    {
        this.bytesRead = bytesRead;
    }

    public ReloadableInputStreamHandler getReloadableInputStreamHandler()
    {
        return this.reloadableInputStreamHandler;
    }

    public void setReloadableInputStreamHandler(ReloadableInputStreamHandler reloadableInputStreamHandler)
    {
        this.reloadableInputStreamHandler = reloadableInputStreamHandler;
    }

    public List<ByteRange> getByteRanges()
    {
        return byteRanges;
    }

    public void setByteRanges(List<ByteRange> byteRanges)
    {
        this.byteRanges = byteRanges;
    }

    public ByteRange getCurrentByteRange()
    {
        return currentByteRange;
    }

    public void setCurrentByteRange(ByteRange currentByteRange)
    {
        this.currentByteRange = currentByteRange;
    }

    public boolean isRangedMode()
    {
        return rangedMode;
    }

    public void setRangedMode(boolean rangedMode)
    {
        this.rangedMode = rangedMode;
    }

}
