package com.veadan.folib.artifact.locator;

import com.veadan.folib.artifact.locator.handlers.ArtifactDirectoryOperation;
import com.veadan.folib.providers.io.RepositoryPath;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author mtodorov
 */
public class ArtifactDirectoryLocator
{

    private static final Logger logger = LoggerFactory.getLogger(ArtifactDirectoryLocator.class);

    private ArtifactDirectoryOperation operation;

    /**
     * The basedir to start analyzing from. Define this only, if you need to pass null values for operation.storage and
     * operation.repository.
     */
    private RepositoryPath basedir;

    public void locateArtifactDirectories()
        throws IOException
    {
        long startTime = System.currentTimeMillis();

        RepositoryPath startingPath = getStartingPath();

        try (Stream<Path> pathStream = Files.walk(startingPath))
        {
            pathStream.filter(Files::isDirectory)
                      // Skip directories which start with a dot (like, for example: .index)
                      .filter(path -> !path.getFileName().toString().startsWith("."))
                      // Note: Sorting can be expensive:
                      .sorted()
                      .forEach(this::execute);
        }

        long endTime = System.currentTimeMillis();

        logger.debug("Executed (cache: {}) visits in {} ms.",
                     -operation.getVisitedRootPaths().size(), (endTime - startTime));

        getOperation().getVisitedRootPaths().clear();
    }

    public RepositoryPath getStartingPath()
    {
        // The root path
        RepositoryPath rootPath =
                basedir != null ? basedir : getOperation().getBasePath().getFileSystem().getRootDirectory();
        if (getOperation().getBasePath() != null)
        {
            rootPath = rootPath.resolve(getOperation().getBasePath().relativize());
        }
        rootPath = rootPath.normalize();

        logger.debug("ArtifactDirectoryLocator started in: path-[{}]", rootPath);

        return Files.isDirectory(rootPath) ? rootPath : rootPath.getParent();
    }

    public ArtifactDirectoryOperation getOperation()
    {
        return operation;
    }

    public void setOperation(ArtifactDirectoryOperation operation)
    {
        this.operation = operation;
    }

    public RepositoryPath getBasedir()
    {
        return basedir;
    }

    public void setBasedir(RepositoryPath basedir)
    {
        this.basedir = basedir;
    }

    void execute(Path path)
    {
        try
        {
            operation.execute((RepositoryPath) path);
        }
        catch (IOException e)
        {
            logger.error("Failed to execute operation [{}]", operation.getClass().getSimpleName(), e);
        }
    }
    
}
