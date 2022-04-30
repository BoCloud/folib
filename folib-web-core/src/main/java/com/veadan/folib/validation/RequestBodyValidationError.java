package com.veadan.folib.validation;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;

/**
 * @author Przemyslaw Fusik
 */
@JsonInclude(JsonInclude.Include.ALWAYS)
public class RequestBodyValidationError
{

    private final String message;

    @JsonSerialize(using = RequestBodyValidationErrorsJsonSerializer.class)
    private MultiValueMap<String, String> errors = new LinkedMultiValueMap<>();

    public RequestBodyValidationError(final String message)
    {
        this.message = message;
    }

    public String getMessage()
    {
        return message;
    }

    public void add(final String field,
                    final String message)
    {
        errors.add(field, message);
    }
}
