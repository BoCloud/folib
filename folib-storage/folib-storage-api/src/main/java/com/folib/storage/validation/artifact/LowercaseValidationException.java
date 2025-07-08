package com.folib.storage.validation.artifact;

/**
 * Created by dinesh on 12/6/17.
 */
public class LowercaseValidationException
        extends ArtifactCoordinatesValidationException
{

    public LowercaseValidationException()
    {

    }

    public LowercaseValidationException(String message)
    {
        super(message);
    }

    public LowercaseValidationException(String message,
                                        Throwable cause)
    {
        super(message, cause);
    }

    public LowercaseValidationException(Throwable cause)
    {
        super(cause);
    }

}

