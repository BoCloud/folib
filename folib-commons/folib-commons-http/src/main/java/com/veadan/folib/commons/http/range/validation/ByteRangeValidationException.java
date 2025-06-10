package com.veadan.folib.commons.http.range.validation;

/**
 * @author Pablo Tirado
 */
public class ByteRangeValidationException
        extends RuntimeException
{

    public ByteRangeValidationException(String message)
    {
        super(message);
    }

    public ByteRangeValidationException(String message,
                                        Throwable cause)
    {
        super(message, cause);
    }

}
