package com.veadan.folib.artifact.archive;

import com.veadan.folib.providers.io.RepositoryPath;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Set;

import org.apache.commons.compress.archivers.ArchiveInputStream;
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream;
import org.apache.commons.compress.compressors.bzip2.BZip2CompressorInputStream;

/**
 * @author veadan
 */
public enum Bzip2ArchiveListingFunction
        implements ArchiveListingFunction
{

    INSTANCE;

    @Override
    public Set<String> listFilenames(final RepositoryPath path)
            throws IOException
    {
        try (InputStream is = Files.newInputStream(path);
             BufferedInputStream bis = new BufferedInputStream(is);
             BZip2CompressorInputStream bzIs = new BZip2CompressorInputStream(bis);
             ArchiveInputStream tarIs = new TarArchiveInputStream(bzIs))
        {
            return getEntryNames(tarIs);
        }
    }

    @Override
    public boolean supports(final RepositoryPath path)
    {
        final Path fileName = path.getFileName();
        return fileName != null && fileName.toString().endsWith("tar.bz2");
    }
}
