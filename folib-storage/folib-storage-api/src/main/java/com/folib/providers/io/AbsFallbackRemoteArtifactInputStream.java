package com.folib.providers.io;

import java.io.IOException;
import java.io.InputStream;

/**
 * @author veadan
 * @date 1/13/2024 12:04
 */
public abstract class AbsFallbackRemoteArtifactInputStream extends InputStream {

  private InputStream target;
    private  RepositoryPath artifactPath;

    protected abstract InputStream intiTarget()  throws IOException;
    private InputStream getTarget()
            throws IOException
    {
        if (target != null)
        {
            return target;
        }

//        target = getConnection().getResponse().readEntity(InputStream.class);
//        if (target == null)
//        {
//            throw new IOException(String.format("Unexpected null as InputStream response for %s.",
//                    resource));
//        }
        target = intiTarget();
        return target;
    }
    public int read()
            throws IOException
    {
        return getTarget().read();
    }

    public int read(byte[] b)
            throws IOException
    {
        return getTarget().read(b);
    }

    public int read(byte[] b,
                    int off,
                    int len)
            throws IOException
    {
        return getTarget().read(b, off, len);
    }

    public long skip(long n)
            throws IOException
    {
        return getTarget().skip(n);
    }

    public int available()
            throws IOException
    {
        return getTarget().available();
    }

    public void mark(int readlimit)
    {
        throw new UnsupportedOperationException();
    }

    public void reset()
            throws IOException
    {
        getTarget().reset();
    }

    public boolean markSupported()
    {
        return false;
    }

    @Override
    public void close()
            throws IOException
    {
        try
        {
            if (target != null)
            {
                target.close();
            }
        } finally
        {
           // closeConnection();
        }
    }

}
