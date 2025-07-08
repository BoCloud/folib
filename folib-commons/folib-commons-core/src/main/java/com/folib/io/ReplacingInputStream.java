package com.folib.io;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Deque;
import java.util.Iterator;
import java.util.LinkedList;

/**
 * This {@link InputStream} decorates a source {@link InputStream} with ability to replace a target chain of bytes with
 * another specified chain of bytes.<br>
 * 
 * For example it can be used as Mulitpart Stream Adapter to change boundary bytes.
 * 
 * @author @author veadan
 * 
 */
public class ReplacingInputStream
        extends BufferedInputStream
{

    Deque<Integer> inQueue = new LinkedList<Integer>();

    Deque<Integer> outQueue = new LinkedList<Integer>();

    final byte[] search, replacement;

    public ReplacingInputStream(InputStream in,
                                byte[] search,
                                byte[] replacement)
    {
        super(in);
        this.search = search;
        this.replacement = replacement;
    }

    private boolean isMatchFound()
    {
        Iterator<Integer> inIter = inQueue.iterator();
        for (int i = 0; i < search.length; i++)
        {
            if (!inIter.hasNext() || search[i] != inIter.next())
            {
                return false;
            }
        }
        return true;
    }

    private void readAhead()
        throws IOException
    {
        // Work up some look-ahead.
        while (inQueue.size() < search.length)
        {
            int next = super.read();
            inQueue.offer(next);
            if (next == -1)
            {
                break;
            }
        }
    }

    @Override
    public int read()
        throws IOException
    {

        // Next byte already determined.
        if (outQueue.isEmpty())
        {

            readAhead();

            if (isMatchFound())
            {
                for (int i = 0; i < search.length; i++)
                {
                    inQueue.remove();
                }

                for (byte b : replacement)
                {
                    outQueue.offer((int) b);
                }
            }
            else
            {
                outQueue.add(inQueue.remove());
            }
        }

        return outQueue.remove();
    }

    /**
     * Returns false. REFilterInputStream does not support mark() and reset() methods.
     */
    @Override
    public boolean markSupported()
    {
        return false;
    }

    /**
     * Reads from the stream into the provided array.
     */
    @Override
    public int read(byte[] b,
                    int off,
                    int len)
        throws IOException
    {
        int i;
        int ok = 0;
        while (len-- > 0)
        {
            i = read();
            if (i == -1)
            {
                return (ok == 0) ? -1 : ok;
            }
            b[off++] = (byte) i;
            ok++;
        }
        return ok;
    }

    @Override
    public int read(byte[] buffer)
        throws IOException
    {

        return read(buffer, 0, buffer.length);
    }
}
