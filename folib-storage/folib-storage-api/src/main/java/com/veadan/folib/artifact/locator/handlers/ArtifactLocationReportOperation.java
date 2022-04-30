package com.veadan.folib.artifact.locator.handlers;

import com.veadan.folib.providers.io.RepositoryPath;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author mtodorov
 * @author stodorov
 */
public class ArtifactLocationReportOperation
        extends AbstractArtifactLocationHandler
{

    private static final Logger logger = LoggerFactory.getLogger(ArtifactLocationReportOperation.class);

    private RepositoryPath previousPath;


    public ArtifactLocationReportOperation()
    {
    }

    public ArtifactLocationReportOperation(RepositoryPath basePath)
    {
        setBasePath(basePath);
    }

    public void execute(RepositoryPath path) throws IOException
    {
        List<Path> filePathList = Files.walk(path)
                                       .filter(p -> !p.getFileName().startsWith(".pom"))
                                                                    .sorted()
                                                                    .collect(Collectors.toList());

        if (filePathList.isEmpty())
        {
            return;
        }
        // Don't enter visited paths (i.e. version directories such as 1.2, 1.3, 1.4...)
        if (getVisitedRootPaths().containsKey(path) && getVisitedRootPaths().get(path).contains(path))
        {
            return;
        }

        if (logger.isDebugEnabled())
        {
            // We're using System.out.println() here for clarity and due to the length of the lines
            System.out.println(path);
        }

        // The current directory is out of the tree
        if (previousPath != null && !path.startsWith(previousPath))
        {
            getVisitedRootPaths().remove(previousPath);
            previousPath = path;
        }

        if (previousPath == null)
        {
            previousPath = path;
        }

        List<RepositoryPath> versionDirectories = getVersionDirectories(path);
        if (versionDirectories != null)
        {
            getVisitedRootPaths().put(path, versionDirectories);

            System.out.println(path);
        }
    }

}
