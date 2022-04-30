package com.veadan.folib.storage;

import java.io.IOException;

/**
 * @author mtodorov
 */
public class ArtifactStorageException
        extends IOException
{

    public ArtifactStorageException()
    {
    }

    public ArtifactStorageException(String message)
    {
        super(message);
    }

    public ArtifactStorageException(String message,
                                    Throwable cause)
    {
        super(message, cause);
    }

    public ArtifactStorageException(Throwable cause)
    {
        super(cause);
    }

}
