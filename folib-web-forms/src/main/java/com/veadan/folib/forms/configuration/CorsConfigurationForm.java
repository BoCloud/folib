package com.veadan.folib.forms.configuration;

import com.veadan.folib.configuration.CorsConfiguration;
import com.veadan.folib.configuration.MutableCorsConfiguration;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class CorsConfigurationForm
{

    private List<String> allowedOrigins = new ArrayList<>();

    public CorsConfigurationForm()
    {
    }

    public CorsConfigurationForm(List<String> allowedOrigins)
    {
        this.allowedOrigins = allowedOrigins;
    }

    public List<String> getAllowedOrigins()
    {
        return allowedOrigins;
    }

    public void setAllowedOrigins(List<String> allowedOrigins)
    {
        this.allowedOrigins = allowedOrigins;
    }

    public static CorsConfigurationForm fromConfiguration(CorsConfiguration source)
    {
        CorsConfiguration configuration = Optional.ofNullable(source).orElse(
                new CorsConfiguration(new MutableCorsConfiguration())
        );

        return new CorsConfigurationForm(configuration.getAllowedOrigins());
    }
}
