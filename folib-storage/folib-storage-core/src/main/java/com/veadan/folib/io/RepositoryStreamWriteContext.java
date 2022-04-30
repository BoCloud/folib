package com.veadan.folib.io;

import java.io.OutputStream;

public class RepositoryStreamWriteContext extends RepositoryStreamContext
{

    private OutputStream stream;

    public OutputStream getStream()
    {
        return stream;
    }

    public void setStream(OutputStream stream)
    {
        this.stream = stream;
    }

}
