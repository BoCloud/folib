package com.veadan.folib.yaml;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.util.Objects;

import com.veadan.folib.util.ServiceLoaderUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.GenericTypeResolver;
import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.Resource;

import com.fasterxml.jackson.dataformat.yaml.YAMLMapper;

/**
 * @author veadan
 * @author Veadan
 * @author cbono
 */
public abstract class YamlFileManager<T>
{
    private final Logger logger = LoggerFactory.getLogger(getClass());

    private final Class<T> myClazz;

    private final YAMLMapper yamlMapper;

    protected YamlFileManager(YAMLMapperFactory yamlMapperFactory)
    {
        this(yamlMapperFactory, new Class[0]);
    }

    @SuppressWarnings("unchecked")
    protected YamlFileManager(YAMLMapperFactory yamlMapperFactory, Class<?>... classes)
    {
        myClazz = (Class<T>) GenericTypeResolver.resolveTypeArgument(getClass(), YamlFileManager.class);
        yamlMapper = yamlMapperFactory.create(ServiceLoaderUtils.load(classes));
    }

    protected abstract Resource getResource();

    public synchronized void store(final T configuration) throws IOException
    {
        Objects.nonNull(configuration);

        Resource resource = getResource();

        //Check that target resource stored on FS and not under classpath for example
        if (!resource.isFile() || resource instanceof ClassPathResource)
        {
            logger.warn("Skip resource store [{}]", resource);
            return;
        }

        // Write the content - we know its a file at this point - use resource.getFile to work w/ Windows
        try (OutputStream os = new BufferedOutputStream(Files.newOutputStream(resource.getFile().toPath())))
        {
            yamlMapper.writeValue(os, configuration);
        }
    }

    public synchronized T read() throws IOException
    {
        Resource resource = getResource();
        if (!resource.exists())
        {
            return null;
        }

        // Read the content - use FileInputStream for file resources to work w/ Windows
        try (InputStream inputStream = new BufferedInputStream(resource.isFile() ?
                                                               new FileInputStream(resource.getFile()) :
                                                               resource.getInputStream()))
        {
            return yamlMapper.readValue(inputStream, myClazz);
        }
    }
}
