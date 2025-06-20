package com.veadan.folib.dto.configuration;

import com.veadan.folib.configuration.CorsConfiguration;
import com.veadan.folib.configuration.MutableCorsConfiguration;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class CorsConfigurationDto
{

    private List<String> allowedOrigins = new ArrayList<>();

    public CorsConfigurationDto()
    {
    }

    public CorsConfigurationDto(List<String> allowedOrigins)
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

    public static CorsConfigurationDto fromConfiguration(CorsConfiguration source)
    {
        CorsConfiguration configuration = Optional.ofNullable(source).orElse(
                new CorsConfiguration(new MutableCorsConfiguration())
        );

        return new CorsConfigurationDto(configuration.getAllowedOrigins());
    }
}
