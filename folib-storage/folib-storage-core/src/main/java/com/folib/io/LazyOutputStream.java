package com.folib.io;

import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.OutputStream;

/**
 * Allows to create {@link OutputStream} instance with "lazy" initialization.
 * This mean that underlaing {@link OutputStream} instance will be created only
 * when writing directly occures. <br>
 * For example this needed if resource should be locked before writing.
 * 
 * @author veadan
 *
 */
public class LazyOutputStream extends FilterOutputStream
{
    private static final String ERROR_FAILED_TO_CREATE_OUTPUT_STREAM = "Failed to create OutputStream.";

    private OutputStreamSupplier supplier;

    public LazyOutputStream(OutputStreamSupplier supplier)
    {
        super(null);
        this.supplier = supplier;
    }

    @Override
    public void write(int b)
        throws IOException
    {
        init();
        out.write(b);
    }

    @Override
    public void write(byte[] b)
        throws IOException
    {
        init();
        out.write(b);
    }

    @Override
    public void write(byte[] b,
                      int off,
                      int len)
        throws IOException
    {
        init();
        out.write(b, off, len);
    }

    @Override
    public void flush()
        throws IOException
    {
        init();
        out.flush();
    }

    @Override
    public void close()
        throws IOException
    {
        if (out != null)
        {
            out.close();
        }
    }

    public void init()
        throws IOException
    {
        if (out != null)
        {
            return;
        }

        try
        {
            out = supplier.get();
        }
        catch (IOException e)
        {
            throw e;
        }
        catch (Exception e)
        {
            throw new IOException(ERROR_FAILED_TO_CREATE_OUTPUT_STREAM, e);
        } finally
        {
            supplier = null;
        }
    }

    @FunctionalInterface
    public static interface OutputStreamSupplier
    {

        OutputStream get()
            throws IOException;

    }
}
