package com.folib.controllers.support;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * @author Veadan
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class ResponseEntityBody
{

    @JsonProperty("message")
    private String message;

    @JsonCreator
    public ResponseEntityBody(@JsonProperty("message") String message)
    {
        this.message = message;
    }

    public String getMessage()
    {
        return message;
    }

    public void setMessage(String message)
    {
        this.message = message;
    }
}
