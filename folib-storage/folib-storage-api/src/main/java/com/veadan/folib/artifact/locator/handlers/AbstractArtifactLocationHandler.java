package com.veadan.folib.artifact.locator.handlers;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Stream;

import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author mtodorov
 * @author stodorov
 */
public abstract class AbstractArtifactLocationHandler
        implements ArtifactDirectoryOperation
{
    private static final Logger logger = LoggerFactory.getLogger(AbstractArtifactLocationHandler.class);
    
    private LinkedHashMap<RepositoryPath, List<RepositoryPath>> visitedRootPaths = new LinkedHashMap<>();

    /**
     * The base path within the repository from where to start scanning for artifacts.
     */
    private RepositoryPath basePath;


    public LinkedHashMap<RepositoryPath, List<RepositoryPath>> getVisitedRootPaths()
    {
        return visitedRootPaths;
    }

    public List<RepositoryPath> getVersionDirectories(RepositoryPath basePath)
        throws IOException
    {
        Set<RepositoryPath> versionDirectorySet = new TreeSet<>();
        try (Stream<Path> pathStream = Files.walk(basePath)) {
            pathStream
                    .forEach(p -> {
                        if (isMetadata(p))
                        {
                            versionDirectorySet.add((RepositoryPath) p.getParent());
                        }
                    });

            return new ArrayList<>(versionDirectorySet);
        }
    }

    protected boolean isMetadata(Path path)
    {
        try
        {
            return Boolean.TRUE.equals(RepositoryFiles.isMetadata((RepositoryPath) path));
        }
        catch (IOException e)
        {
            logger.error("Failed to read Path attributes for [{}]", path, e);
            return false;
        }
    }

    @Override
    public RepositoryPath getBasePath()
    {
        return basePath;
    }

    public void setBasePath(RepositoryPath basePath)
    {
        this.basePath = basePath;
    }
    
}
