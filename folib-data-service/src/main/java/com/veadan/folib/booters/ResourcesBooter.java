package com.veadan.folib.booters;

import java.io.IOException;
import java.lang.reflect.UndeclaredThrowableException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationContextInitializer;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.core.env.ConfigurableEnvironment;
import org.springframework.core.io.Resource;
import org.springframework.core.io.support.PathMatchingResourcePatternResolver;

/**
 * @author mtodorov
 */
public class ResourcesBooter implements ApplicationContextInitializer<ConfigurableApplicationContext>
{

    private static final Logger logger = LoggerFactory.getLogger(ResourcesBooter.class);

    private PathMatchingResourcePatternResolver resolver = new PathMatchingResourcePatternResolver();

    @Override
    public void initialize(ConfigurableApplicationContext applicationContext)
    {
        ConfigurableEnvironment environment = applicationContext.getEnvironment();
        try
        {
            copyConfigurationFilesFromClasspath(environment.getProperty("folib.home"), "etc/conf");
        }
        catch (IOException e)
        {
            throw new UndeclaredThrowableException(e);
        }
    }

    public Resource[] getConfigurationResourcesFromClasspath(String resourcesBasedir)
            throws IOException
    {
        return resolver.getResources("classpath*:" + resourcesBasedir + "/**/*");
    }

    public Resource[] getResourcesExistingOnClasspathOnly(Resource[] resources)
            throws IOException
    {
        List<Resource> diff = new ArrayList<>();

        for (Resource resource : resources)
        {
            logger.info(resource.getURL().toString());
            diff.add(resource);
        }

        return diff.toArray(new Resource[diff.size()]);
    }

    public void copyConfigurationFilesFromClasspath(String folibHome, String resourcesBasedir)
            throws IOException
    {
        final Resource[] resources = getResourcesExistingOnClasspathOnly(
                getConfigurationResourcesFromClasspath(resourcesBasedir));

        final Path configDir = Paths.get(folibHome, resourcesBasedir);
        if (!Files.exists(configDir))
        {
            Files.createDirectories(configDir);
        }

        for (Resource resource : resources)
        {
            Path destFile = configDir.resolve(resource.getFilename());

            if (Files.exists(destFile))
            {
                logger.info("Resource already exists, skip [{}].", destFile);
                continue;
            }

            Files.copy(resource.getInputStream(), destFile);
        }
    }

    public PathMatchingResourcePatternResolver getResolver()
    {
        return resolver;
    }

    public void setResolver(PathMatchingResourcePatternResolver resolver)
    {
        this.resolver = resolver;
    }

}
