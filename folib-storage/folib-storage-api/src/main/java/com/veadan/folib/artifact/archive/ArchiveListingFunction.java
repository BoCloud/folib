package com.veadan.folib.artifact.archive;

import com.veadan.folib.providers.io.RepositoryPath;

import java.io.IOException;
import java.util.HashSet;
import java.util.Set;

import org.apache.commons.compress.archivers.ArchiveEntry;
import org.apache.commons.compress.archivers.ArchiveInputStream;

/**
 * @author Przemyslaw Fusik
 */
@FunctionalInterface
public interface ArchiveListingFunction
{

    Set<String> listFilenames(RepositoryPath path)
            throws IOException;

    default Set<String> getEntryNames(final ArchiveInputStream archiveInputStream)
            throws IOException
    {
        final Set<String> result = new HashSet<>();
        ArchiveEntry entry;
        while ((entry = archiveInputStream.getNextEntry()) != null)
        {
            result.add(entry.getName());
        }
        return result;
    }

    default boolean supports(RepositoryPath path)
    {
        return true;
    }
}
