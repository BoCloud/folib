package com.veadan.folib.configuration;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * @author veadan
 * @author Veadan
 */
public class MutableCorsConfiguration
        implements Serializable
{

    private List<String> allowedOrigins;

    private List<String> allowedMethods;

    private List<String> allowedHeaders;

    private List<String> exposedHeaders;

    @JsonProperty("allowedCredentials")
    private Boolean allowCredentials;

    private Long maxAge;

    @JsonCreator
    public MutableCorsConfiguration()
    {
    }

    @JsonCreator
    public MutableCorsConfiguration(@JsonProperty("allowedOrigins") List<String> allowedOrigins)
    {
        this.allowedOrigins = allowedOrigins;
    }

    public List<String> getAllowedOrigins()
    {
        return allowedOrigins;
    }

    public void setAllowedOrigins(final List<String> allowedOrigins)
    {
        this.allowedOrigins = allowedOrigins;
    }

    public List<String> getAllowedMethods()
    {
        return allowedMethods;
    }

    public void setAllowedMethods(final List<String> allowedMethods)
    {
        this.allowedMethods = allowedMethods;
    }

    public List<String> getAllowedHeaders()
    {
        return allowedHeaders;
    }

    public void setAllowedHeaders(final List<String> allowedHeaders)
    {
        this.allowedHeaders = allowedHeaders;
    }

    public List<String> getExposedHeaders()
    {
        return exposedHeaders;
    }

    public void setExposedHeaders(final List<String> exposedHeaders)
    {
        this.exposedHeaders = exposedHeaders;
    }

    public Boolean getAllowCredentials()
    {
        return allowCredentials;
    }

    public void setAllowCredentials(final Boolean allowCredentials)
    {
        this.allowCredentials = allowCredentials;
    }

    public Long getMaxAge()
    {
        return maxAge;
    }

    public void setMaxAge(final Long maxAge)
    {
        this.maxAge = maxAge;
    }
}
