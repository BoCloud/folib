package com.veadan.folib.io;

import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;

/**
 * Allows to create {@link InputStream} instance with "lazy" initialization.
 * This mean that underlaing {@link InputStream} instance will be created only
 * when reading directly occures. <br>
 * For example this needed if resource should be locked before reading.
 * 
 * @author xuxinping
 */
public class LazyInputStream extends FilterInputStream
{

    private static final String ERROR_FAILED_TO_CREATE_INPUT_STREAM = "Failed to create InputStream.";

    private InputStreamSupplier supplier;

    public LazyInputStream(InputStreamSupplier supplier)
    {
        super(null);
        this.supplier = supplier;
    }

    @Override
    public int read()
        throws IOException
    {
        init();
        return in.read();
    }

    @Override
    public int read(byte[] b)
        throws IOException
    {
        init();
        return in.read(b);
    }

    @Override
    public int read(byte[] b,
                    int off,
                    int len)
        throws IOException
    {
        init();
        return in.read(b, off, len);
    }

    @Override
    public long skip(long n)
        throws IOException
    {
        init();
        return in.skip(n);
    }

    @Override
    public int available()
        throws IOException
    {
        init();
        return in.available();
    }

    @Override
    public void close()
        throws IOException
    {
        if (in == null)
        {
            return;
        }
        in.close();
    }

    @Override
    public synchronized void mark(int readlimit)
    {
        in.mark(readlimit);
    }

    @Override
    public synchronized void reset()
        throws IOException
    {
        init();
        in.reset();
    }

    @Override
    public boolean markSupported()
    {
        return in.markSupported();
    }

    public void init()
        throws IOException
    {
        if (in != null)
        {
            return;
        }

        try
        {
            in = supplier.get();
        }
        catch (IOException e)
        {
            throw e;
        }
        catch (Exception e)
        {
            throw new IOException(ERROR_FAILED_TO_CREATE_INPUT_STREAM, e);
        }
        finally
        {
            supplier = null;
        }
    }

    @FunctionalInterface
    public static interface InputStreamSupplier
    {

        InputStream get()
            throws IOException;

    }

}
