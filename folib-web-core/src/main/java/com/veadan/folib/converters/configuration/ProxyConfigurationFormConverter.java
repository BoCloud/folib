package com.veadan.folib.converters.configuration;

import com.veadan.folib.dto.configuration.ProxyConfigurationDto;
import com.veadan.folib.configuration.MutableProxyConfiguration;

import org.springframework.core.convert.converter.Converter;

/**
 * @author Veadan
 */
public enum ProxyConfigurationFormConverter
        implements Converter<ProxyConfigurationDto, MutableProxyConfiguration>
{

    INSTANCE;

    @Override
    public MutableProxyConfiguration convert(ProxyConfigurationDto proxyConfigurationForm)
    {
        MutableProxyConfiguration proxyConfiguration = new MutableProxyConfiguration();
        proxyConfiguration.setHost(proxyConfigurationForm.getHost());
        proxyConfiguration.setPort(proxyConfigurationForm.getPort());
        proxyConfiguration.setType(proxyConfigurationForm.getType());
        proxyConfiguration.setUsername(proxyConfigurationForm.getUsername());
        proxyConfiguration.setPassword(proxyConfigurationForm.getPassword());
        proxyConfiguration.setNonProxyHosts(proxyConfigurationForm.getNonProxyHosts());

        return proxyConfiguration;
    }
}
