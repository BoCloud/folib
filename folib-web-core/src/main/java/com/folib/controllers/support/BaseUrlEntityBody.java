package com.folib.controllers.support;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * @author Veadan
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class BaseUrlEntityBody
{

    @JsonProperty("baseUrl")
    private String baseUrl;

    @JsonCreator
    public BaseUrlEntityBody(@JsonProperty("baseUrl") String baseUrl)
    {
        this.baseUrl = baseUrl;
    }

    public String getBaseUrl()
    {
        return baseUrl;
    }

    public void setBaseUrl(String baseUrl)
    {
        this.baseUrl = baseUrl;
    }
}
