package com.veadan.folib.storage.metadata.maven.visitors;

import java.io.IOException;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author stodorov
 */
public class ArtifactVersionDirectoryVisitor
        extends SimpleFileVisitor<Path>
{

    private static final String CHECKSUM_PATTERN = "glob:*.{md5,sha1,sha256}";
    private static final String METADATA_PATTERN = "glob:maven-metadata.*";
    private static final String SNAPSHOT_PATTERN = "glob:*SNAPSHOT*";

    private List<Path> matchingPaths = new ArrayList<>();

    private PathMatcher checksumFileMatcher;
    private PathMatcher metadataFileMatcher;
    private PathMatcher snapshotFileMatcher;

    private static final Logger logger = LoggerFactory.getLogger(ArtifactVersionDirectoryVisitor.class);


    public ArtifactVersionDirectoryVisitor()
    {
        checksumFileMatcher = FileSystems.getDefault().getPathMatcher(CHECKSUM_PATTERN);
        metadataFileMatcher = FileSystems.getDefault().getPathMatcher(METADATA_PATTERN);
        snapshotFileMatcher = FileSystems.getDefault().getPathMatcher(SNAPSHOT_PATTERN);
    }

    @Override
    public FileVisitResult visitFile(Path file,
                                     BasicFileAttributes attr)
            throws IOException
    {
        Path name = file.getFileName();
        if (!checksumFileMatcher.matches(name) && !metadataFileMatcher.matches(name))
        {
            if (!snapshotFileMatcher.matches(name))
            {
                matchingPaths.add(file);
            }
            else
            {
                //
                // TODO: Make it possible to configure what should be done when
                //       a snapshot version directory contains both 1.2-SNAPSHOT
                //       and timestamped versions such as 1.2-20150507.013444-1.
                //
                // Current action: Don't add matching file to matched paths and log a warning message
                // Result: Generates metadata as if the directory contains only timestamped artifacts (if any)
                //
                logger.warn("Snapshot artifact name contains SNAPSHOT instead of timestamp: {}", file.toAbsolutePath());
            }
        }

        return FileVisitResult.CONTINUE;
    }

    public List<Path> getMatchingPaths(){
        return matchingPaths;
    }

}
