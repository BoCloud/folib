package com.folib.storage.metadata.maven.io.filters;

import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * @author veadan
 * @author veadan
 */
public class ArtifactVersionDirectoryFilter
        implements DirectoryStream.Filter<Path>
{

    private boolean excludeHiddenDirectories = true;

    private boolean containsMetadataFiles(Path path)
            throws IOException
    {
        if (Files.isDirectory(path))
        {
            try (DirectoryStream<Path> ds = Files.newDirectoryStream(path, PomPathnameFilter.INSTANCE))
            {
                return ds.iterator().hasNext();
            }
        }

        return false;
    }

    @Override
    public boolean accept(final Path entry)
            throws IOException
    {
        return (entry != null && Files.isDirectory(entry) &&
                (!excludeHiddenDirectories || !entry.getFileName().toString().matches(".*//*\\..*"))) &&
               containsMetadataFiles(entry);

    }

    public void setExcludeHiddenDirectories(final boolean excludeHiddenDirectories)
    {
        this.excludeHiddenDirectories = excludeHiddenDirectories;
    }

    private static class PomPathnameFilter
            implements DirectoryStream.Filter<Path>
    {

        private static final PomPathnameFilter INSTANCE = new PomPathnameFilter();

        @Override
        public boolean accept(final Path entry)
                throws IOException
        {
            return entry.toString().endsWith(".pom");
        }
    }
}
