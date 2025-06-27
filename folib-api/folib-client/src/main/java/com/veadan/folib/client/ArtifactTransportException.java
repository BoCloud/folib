package com.veadan.folib.client;

/**
 * This exception is thrown during the resolution or deployment of artifacts.
 *
 * @author veadan
 */
public class ArtifactTransportException
        extends Exception
{

    public ArtifactTransportException()
    {
    }

    public ArtifactTransportException(String message)
    {
        super(message);
    }

    public ArtifactTransportException(String message,
                                      Throwable cause)
    {
        super(message, cause);
    }

    public ArtifactTransportException(Throwable cause)
    {
        super(cause);
    }

    public ArtifactTransportException(String message,
                                      Throwable cause,
                                      boolean enableSuppression,
                                      boolean writableStackTrace)
    {
        super(message, cause, enableSuppression, writableStackTrace);
    }

}
