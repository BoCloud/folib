package com.veadan.folib.authentication.registry.support;

import com.veadan.folib.resource.ConfigurationResourceResolver;
import com.veadan.folib.util.ClassLoaderFactory;

import javax.inject.Inject;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.lang.reflect.UndeclaredThrowableException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashSet;
import java.util.Set;
import java.util.regex.Pattern;

import org.apache.commons.collections4.CollectionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
public class ExternalAuthenticatorsHelper
{

    private static final Pattern AUTHENTICAION_PROVIDER_PATTERN = Pattern.compile(
            "folib-.*-authentication-provider.*jar");

    private static final Logger logger = LoggerFactory.getLogger(ExternalAuthenticatorsHelper.class);

    @Inject
    private ConfigurationResourceResolver configurationResourceResolver;


    private ExternalAuthenticatorsHelper()
    {

    }

    public ClassLoader getExternalAuthenticatorsClassLoader(ClassLoader parent)
    {
        Path authenticatorsDirectory;
        try
        {
            authenticatorsDirectory = Paths.get(
                    configurationResourceResolver.getConfigurationResource("folib.authentication.lib",
                                                                           "webapp/WEB-INF/lib").getURI());
        }
        catch (FileNotFoundException e)
        {
            logger.debug(e.getMessage());
            return parent;
        }
        catch (IOException e)
        {
            throw new UndeclaredThrowableException(e);
        }
        if (!Files.exists(authenticatorsDirectory))
        {
            logger.debug("{} does not exist.", authenticatorsDirectory);
            return parent;
        }
        if (!Files.isDirectory(authenticatorsDirectory))
        {
            logger.error("{} is not a directory.", authenticatorsDirectory);
            return parent;
        }

        final Set<Path> authenticatorsJarPaths = getAuthenticatorJarPaths(authenticatorsDirectory);

        if (CollectionUtils.isEmpty(authenticatorsJarPaths))
        {
            logger.debug("{} does not contain any authenticator jar files.", authenticatorsDirectory);
            return parent;
        }

        return ClassLoaderFactory.urlClassLoaderFromPaths(parent, authenticatorsJarPaths);
    }

    private static Set<Path> getAuthenticatorJarPaths(final Path authenticatorsDirectory)
    {
        final Set<Path> authenticatorsJarPaths = new HashSet<>();
        try (DirectoryStream<Path> directoryStream = Files.newDirectoryStream(authenticatorsDirectory))
        {
            for (Path path : directoryStream)
            {
                if (isAuthenticatorJarPath(path))
                {
                    authenticatorsJarPaths.add(path);
                }
            }
        }
        catch (IOException e)
        {
            throw new UndeclaredThrowableException(e);
        }
        return authenticatorsJarPaths;
    }

    private static boolean isAuthenticatorJarPath(Path path)
    {
        return path != null && !Files.isDirectory(path) &&
               AUTHENTICAION_PROVIDER_PATTERN.matcher(path.getFileName().toString()).matches();
    }

}
