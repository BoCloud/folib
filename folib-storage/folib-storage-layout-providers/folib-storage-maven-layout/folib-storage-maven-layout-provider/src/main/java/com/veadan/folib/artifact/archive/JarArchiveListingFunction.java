package com.veadan.folib.artifact.archive;

import com.veadan.folib.providers.io.RepositoryPath;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Set;

import org.apache.commons.compress.archivers.ArchiveInputStream;
import org.apache.commons.compress.archivers.jar.JarArchiveInputStream;

/**
 * @author veadan
 */
public enum JarArchiveListingFunction
        implements ArchiveListingFunction
{
    INSTANCE;

    @Override
    public Set<String> listFilenames(final RepositoryPath path)
            throws IOException
    {
        try (InputStream is = Files.newInputStream(path);
             BufferedInputStream bis = new BufferedInputStream(is);
             ArchiveInputStream ais = new JarArchiveInputStream(bis))
        {
            return getEntryNames(ais);
        }
    }

    @Override
    public boolean supports(final RepositoryPath path)
    {
        final Path fileName = path.getFileName();
        if (fileName == null)
        {
            return false;
        }
        final String filenameString = fileName.toString();
        return filenameString.endsWith("jar") ||
               filenameString.endsWith("war") ||
               filenameString.endsWith("ear") ||
               filenameString.endsWith("zip");
    }
}
