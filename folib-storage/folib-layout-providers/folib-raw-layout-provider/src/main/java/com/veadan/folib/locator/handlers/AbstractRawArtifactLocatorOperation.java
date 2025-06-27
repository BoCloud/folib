package com.veadan.folib.locator.handlers;

import com.veadan.folib.artifact.locator.handlers.AbstractArtifactLocationHandler;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.storage.metadata.VersionCollectionRequest;
import com.veadan.folib.storage.metadata.VersionCollector;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author veadan
 */
public abstract class AbstractRawArtifactLocatorOperation
        extends AbstractArtifactLocationHandler {

    private static final Logger logger = LoggerFactory.getLogger(AbstractRawArtifactLocatorOperation.class);

    private RepositoryPath previousPath;


    public AbstractRawArtifactLocatorOperation() {
    }

    @Override
    public void execute(RepositoryPath direcotryPath)
            throws IOException {
        List<Path> pomFiles;
        try (Stream<Path> pathStream = Files.list(direcotryPath)) {
            pomFiles = pathStream.filter(p -> p.getFileName().toString().endsWith(".pom")).sorted().collect(
                    Collectors.toList());
        }

        if (pomFiles.isEmpty()) {
            return;
        }

        RepositoryPath artifactGroupDirectoryPath = direcotryPath.getParent();

        // Don't enter visited paths (i.e. version directories such as 1.2, 1.3, 1.4...)
        if (getVisitedRootPaths().containsKey(artifactGroupDirectoryPath) && getVisitedRootPaths().get(artifactGroupDirectoryPath).contains(direcotryPath)) {
            return;
        }

        if (logger.isDebugEnabled()) {
            // We're using System.out.println() here for clarity and due to the length of the lines
            System.out.println(artifactGroupDirectoryPath);
        }

        // The current directory is out of the tree
        if (previousPath != null && !artifactGroupDirectoryPath.startsWith(previousPath)) {
            getVisitedRootPaths().remove(previousPath);
            previousPath = artifactGroupDirectoryPath;
        }

        if (previousPath == null) {
            previousPath = artifactGroupDirectoryPath;
        }

        List<RepositoryPath> versionDirectories = getVersionDirectories(artifactGroupDirectoryPath);
        if (versionDirectories == null) {
            return;
        }
        getVisitedRootPaths().put(artifactGroupDirectoryPath, versionDirectories);

        VersionCollector versionCollector = new VersionCollector();
        VersionCollectionRequest request = versionCollector.collectVersions(artifactGroupDirectoryPath.toAbsolutePath());

        if (logger.isDebugEnabled()) {
            for (RepositoryPath directory : versionDirectories) {
                // We're using System.out.println() here for clarity and due to the length of the lines
                System.out.println(" " + directory.toAbsolutePath());
            }
        }

        executeOperation(request, artifactGroupDirectoryPath, versionDirectories);
    }

    public abstract void executeOperation(VersionCollectionRequest request,
                                          RepositoryPath artifactGroupDirectoryPath,
                                          List<RepositoryPath> versionDirectories)
            throws IOException;

}
