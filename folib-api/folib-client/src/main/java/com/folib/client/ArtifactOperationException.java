package com.folib.client;

/**
 * This exception is thrown during the management of artifacts.
 *
 * @author veadan
 */
public class ArtifactOperationException extends Exception
{

    public ArtifactOperationException()
    {
    }

    public ArtifactOperationException(String message)
    {
        super(message);
    }

    public ArtifactOperationException(String message,
                                      Throwable cause)
    {
        super(message, cause);
    }

    public ArtifactOperationException(Throwable cause)
    {
        super(cause);
    }

    public ArtifactOperationException(String message,
                                      Throwable cause,
                                      boolean enableSuppression,
                                      boolean writableStackTrace)
    {
        super(message, cause, enableSuppression, writableStackTrace);
    }

}
