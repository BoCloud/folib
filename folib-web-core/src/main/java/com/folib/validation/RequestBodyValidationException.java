package com.folib.validation;

import org.springframework.validation.Errors;

/**
 * @author veadan
 */
public class RequestBodyValidationException
        extends RuntimeException
{

    private final Errors errors;

    public RequestBodyValidationException(final String message,
                                          final Errors errors)
    {
        super(message);
        this.errors = errors;
    }

    public Errors getErrors()
    {
        return errors;
    }
}
