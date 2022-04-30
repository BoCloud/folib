package com.veadan.folib.util;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.Path;
import java.util.Collections;

import org.apache.commons.lang.StringUtils;

/**
 * @author Przemyslaw Fusik
 */
public class JarFileUtils
{

    public static Path resolvePathInJar(String pathInJar)
            throws URISyntaxException, IOException
    {
        URI resource = JarFileUtils.class.getResource("").toURI();
        FileSystem fileSystem = FileSystems.newFileSystem(resource, Collections.<String, String>emptyMap());
        String pathInJarWithout = StringUtils.substringAfterLast(pathInJar, "jar!");
        pathInJarWithout = pathInJarWithout.replace("!", "");
        return fileSystem.getPath(pathInJarWithout);
    }
}
