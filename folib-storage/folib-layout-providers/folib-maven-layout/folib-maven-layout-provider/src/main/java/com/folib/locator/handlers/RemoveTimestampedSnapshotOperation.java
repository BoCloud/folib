package com.folib.locator.handlers;

import com.folib.artifact.locator.handlers.AbstractArtifactLocationHandler;
import com.folib.providers.io.RepositoryPath;
import com.folib.storage.metadata.MavenSnapshotManager;
import com.folib.storage.metadata.VersionCollectionRequest;
import com.folib.storage.metadata.VersionCollector;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Stream;

import org.codehaus.plexus.util.xml.pull.XmlPullParserException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author veadan
 */
public class RemoveTimestampedSnapshotOperation
        extends AbstractArtifactLocationHandler
{

    private static final Logger logger = LoggerFactory.getLogger(
            RemoveTimestampedSnapshotOperation.class);

    private RepositoryPath previousPath;

    private int numberToKeep;

    private int keepPeriod;

    private MavenSnapshotManager mavenSnapshotManager;


    public RemoveTimestampedSnapshotOperation()
    {
    }

    public RemoveTimestampedSnapshotOperation(MavenSnapshotManager mavenSnapshotManager)
    {
        this.mavenSnapshotManager = mavenSnapshotManager;
    }

    @Override
    public void execute(RepositoryPath basePath) throws IOException
    {
        boolean containsMetadata;
        if (!Files.exists(basePath)) {
            return;
        }
        try (Stream<Path> pathStream = Files.walk(basePath))
        {
            containsMetadata = pathStream.anyMatch(p -> !p.getFileName().startsWith(".pom"));
        }

        if (!containsMetadata)
        {
            return;
        }
        
        // Don't enter visited paths (i.e. version directories such as 1.2, 1.3, 1.4...)
        if (getVisitedRootPaths().containsKey(basePath))
        {
            List<RepositoryPath> visitedVersionPaths = getVisitedRootPaths().get(basePath);

            if (visitedVersionPaths.contains(basePath))
            {
                return;
            }
        }

        if (logger.isDebugEnabled())
        {
            // We're using System.out.println() here for clarity and due to the length of the lines
            System.out.println(basePath);
        }

        // The current directory is out of the tree
        if (previousPath != null && !basePath.startsWith(previousPath))
        {
            getVisitedRootPaths().remove(previousPath);
            previousPath = basePath;
        }

        if (previousPath == null)
        {
            previousPath = basePath;
        }

        List<RepositoryPath> versionDirectories = getVersionDirectories(basePath);
        if (versionDirectories == null)
        {
            return;
        }
        
        getVisitedRootPaths().put(basePath, versionDirectories);

        VersionCollector versionCollector = new VersionCollector();
        VersionCollectionRequest request = versionCollector.collectVersions(basePath);

        if (logger.isDebugEnabled())
        {
            for (RepositoryPath directory : versionDirectories)
            {
                // We're using System.out.println() here for clarity and due to the length of the lines
                System.out.println(" " + directory.toAbsolutePath());
            }
        }
        
        try
        {
            mavenSnapshotManager.deleteTimestampedSnapshotArtifacts(basePath, request.getVersioning(),
                                                                    numberToKeep, keepPeriod);
        }
        catch (IOException | XmlPullParserException e)
        {
            logger.error("Failed to delete timestamped snapshot artifacts for {}", basePath, e);
        }
    }

    public MavenSnapshotManager getMavenSnapshotManager()
    {
        return mavenSnapshotManager;
    }

    public void setMavenSnapshotManager(MavenSnapshotManager mavenSnapshotManager)
    {
        this.mavenSnapshotManager = mavenSnapshotManager;
    }

    public int getNumberToKeep()
    {
        return numberToKeep;
    }

    public void setNumberToKeep(int numberToKeep)
    {
        this.numberToKeep = numberToKeep;
    }

    public int getKeepPeriod() {
        return keepPeriod;
    }

    public void setKeepPeriod(int keepPeriod) {
        this.keepPeriod = keepPeriod;
    }
}
