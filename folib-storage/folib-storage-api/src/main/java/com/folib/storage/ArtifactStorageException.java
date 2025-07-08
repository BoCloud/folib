package com.folib.storage;

import java.io.IOException;

/**
 * @author veadan
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
