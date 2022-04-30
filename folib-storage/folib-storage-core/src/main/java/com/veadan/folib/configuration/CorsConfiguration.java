package com.veadan.folib.configuration;

import javax.annotation.concurrent.Immutable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import java.util.Collections;
import java.util.List;

import com.google.common.collect.ImmutableList;
import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;

/**
 * @author Przemyslaw Fusik
 */
@Immutable
@XmlAccessorType(XmlAccessType.FIELD)
@SuppressFBWarnings(value = "AJCIP_FIELD_ISNT_FINAL_IN_IMMUTABLE_CLASS")
public class CorsConfiguration
{

    private List<String> allowedOrigins;

    private List<String> allowedMethods;

    private List<String> allowedHeaders;

    private List<String> exposedHeaders;

    private Boolean allowCredentials;

    private Long maxAge;

    CorsConfiguration()
    {

    }

    public CorsConfiguration(final MutableCorsConfiguration source)
    {
        this.allowedOrigins = immuteList(source.getAllowedOrigins());
        this.allowedMethods = immuteList(source.getAllowedMethods());
        this.allowedHeaders = immuteList(source.getAllowedHeaders());
        this.exposedHeaders = immuteList(source.getExposedHeaders());
        this.allowCredentials = source.getAllowCredentials();
        this.maxAge = source.getMaxAge();
    }

    private <T> List<T> immuteList(final List<T> source)
    {
        return source != null ? ImmutableList.copyOf(source) : Collections.emptyList();
    }

    public List<String> getAllowedOrigins()
    {
        return allowedOrigins;
    }

    public List<String> getAllowedMethods()
    {
        return allowedMethods;
    }

    public List<String> getAllowedHeaders()
    {
        return allowedHeaders;
    }

    public List<String> getExposedHeaders()
    {
        return exposedHeaders;
    }

    public Boolean getAllowCredentials()
    {
        return allowCredentials;
    }

    public Long getMaxAge()
    {
        return maxAge;
    }
}
