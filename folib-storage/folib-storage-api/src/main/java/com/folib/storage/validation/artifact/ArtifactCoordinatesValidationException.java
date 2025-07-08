package com.folib.storage.validation.artifact;

/**
 * @author Veadan
 */
public class ArtifactCoordinatesValidationException extends Exception
{

    public ArtifactCoordinatesValidationException()
    {
    }

    public ArtifactCoordinatesValidationException(String message)
    {
        super(message);
    }

    public ArtifactCoordinatesValidationException(String message,
                                                  Throwable cause)
    {
        super(message, cause);
    }

    public ArtifactCoordinatesValidationException(Throwable cause)
    {
        super(cause);
    }

    public ArtifactCoordinatesValidationException(String message,
                                                  Throwable cause,
                                                  boolean enableSuppression,
                                                  boolean writableStackTrace)
    {
        super(message, cause, enableSuppression, writableStackTrace);
    }

}
