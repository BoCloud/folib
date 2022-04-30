package com.veadan.folib.storage;

import java.io.IOException;

/**
 * @author mtodorov
 */
public class ArtifactResolutionException
        extends IOException
{

    public ArtifactResolutionException()
    {
    }

    public ArtifactResolutionException(String message)
    {
        super(message);
    }

    public ArtifactResolutionException(String message,
                                       Throwable cause)
    {
        super(message, cause);
    }

    public ArtifactResolutionException(Throwable cause)
    {
        super(cause);
    }

}
