package com.veadan.folib.resource;

import com.veadan.folib.booters.PropertiesBooter;

import javax.inject.Inject;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.env.Environment;
import org.springframework.core.io.DefaultResourceLoader;
import org.springframework.core.io.FileSystemResource;
import org.springframework.core.io.Resource;
import org.springframework.stereotype.Component;

/**
 * @author mtodorov
 */
@Component
public class ConfigurationResourceResolver
{

    private static final Logger logger = LoggerFactory.getLogger(ConfigurationResourceResolver.class);

    @Inject
    private PropertiesBooter propertiesBooter;
    
    @Inject
    private Environment environment;


    public Resource getConfigurationResource(String propertyKey,
                                             String propertyDefaultValue)
    {
        final String configurationPath = propertyDefaultValue != null && !propertyDefaultValue.startsWith("classpath:") ?
                                         propertiesBooter.getHomeDirectory() + "/" + propertyDefaultValue :
                                         propertyDefaultValue;

        return getConfigurationResource(configurationPath, propertyKey, propertyDefaultValue);
    }

    /**
     * @param configurationPath
     *            The configuration file's path. If null, either propertyKey,
     *            or propertyKeyDefaultValue must be specified.
     * @param propertyKey
     *            The system property key to use when trying to load the
     *            configuration.
     * @param propertyDefaultValue
     *            The default property key value.
     * @return
     * @throws IOException
     */
    public Resource getConfigurationResource(String configurationPath,
                                             String propertyKey,
                                             String propertyDefaultValue)
    {
        DefaultResourceLoader resourceLoader = new DefaultResourceLoader();
        resourceLoader.addProtocolResolver((l,
                                            r) -> {

            File file = new File(l);
            if (!file.exists())
            {
                return null;
            }

            return new FileSystemResource(file);
        });

        String filename;
        if ((filename = getProperty(propertyKey)) != null)
        {
            logger.debug("Using configured resource path [{}]", filename);

            return resourceLoader.getResource(filename);
        }

        logger.debug("Try to fetch configuration resource path [{}]", configurationPath);

        if (configurationPath != null &&
            (!configurationPath.startsWith("classpath") && !(Files.exists(Paths.get(configurationPath)))))
        {
            logger.info("Configuration resource [{}] does not exist, will try to resolve with configured location [{}].",
                        configurationPath, propertyKey);

            configurationPath = null;
        }

        if (configurationPath != null)
        {
            logger.debug("Using provided resource path [{}]", configurationPath);

            return resourceLoader.getResource(configurationPath);
        }
        else
        {
            logger.debug("Using default resource path [{}]", propertyDefaultValue);

            return resourceLoader.getResource(propertyDefaultValue);
        }
    }

    private String getProperty(String propertyKey)
    {
        return environment.getProperty(propertyKey);
    }

}
