package com.folib.io;

import java.io.InputStream;

public class RepositoryStreamReadContext extends RepositoryStreamContext
{

    private InputStream stream;

    public InputStream getStream()
    {
        return stream;
    }

    public void setStream(InputStream stream)
    {
        this.stream = stream;
    }

}
