package com.folib.converters.configuration;

import com.folib.forms.configuration.ProxyConfigurationForm;
import com.folib.configuration.MutableProxyConfiguration;

import org.springframework.core.convert.converter.Converter;

/**
 * @author Veadan
 */
public enum ProxyConfigurationFormConverter
        implements Converter<ProxyConfigurationForm, MutableProxyConfiguration>
{

    INSTANCE;

    @Override
    public MutableProxyConfiguration convert(ProxyConfigurationForm proxyConfigurationForm)
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
