package com.veadan.folib.jtwig.extensions;

import org.jtwig.environment.EnvironmentConfigurationBuilder;
import org.jtwig.extension.Extension;

/**
 * 
 * @author veadan
 *
 */
public class ByteSizeConversionExtension implements Extension
{

    @Override
    public void configure(EnvironmentConfigurationBuilder configurationBuilder)
    {
        configurationBuilder.functions().add(new HumanReadableSizeConversionFunction());
    }

}
