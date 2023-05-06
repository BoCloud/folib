package com.veadan.folib.storage.metadata;

import com.veadan.folib.artifact.MavenArtifactUtils;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.storage.repository.Repository;

import javax.inject.Inject;
import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;

import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.artifact.repository.metadata.Metadata;
import org.apache.maven.artifact.repository.metadata.Versioning;
import org.codehaus.plexus.util.xml.pull.XmlPullParserException;
import org.javatuples.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * @author Kate Novik.
 */
@Component
public class MavenSnapshotManager
{

    public static final String TIMESTAMP_FORMAT = "yyyyMMdd.HHmmss";

    private static final Logger logger = LoggerFactory.getLogger(MavenSnapshotManager.class);

    @Inject
    private MavenMetadataManager mavenMetadataManager;

    public MavenSnapshotManager()
    {
    }

    public void deleteTimestampedSnapshotArtifacts(RepositoryPath basePath,
                                                   Versioning versioning,
                                                   int numberToKeep,
                                                   Date keepDate)
            throws IOException,
                   XmlPullParserException
    {
        Repository repository = basePath.getRepository();
        if (!RepositoryFiles.artifactExists(basePath))
        {
            logger.error("Removal of timestamped Maven snapshot artifact: {}.", basePath);
            
            return;
        }
        
        logger.info("Removal of timestamped Maven snapshot artifact {} in '{}:{}'.",
                     basePath, repository.getStorage().getId(), repository.getId());

        Pair<String, String> artifactGroup = MavenArtifactUtils.getDirectoryGA(basePath);
        String artifactGroupId = artifactGroup.getValue0();
        String artifactId = artifactGroup.getValue1();

        if (versioning.getVersions().isEmpty())
        {
            return;
        }
        
        for (String version : versioning.getVersions())
        {

            RepositoryPath versionDirectoryPath = basePath.resolve(ArtifactUtils.toSnapshotVersion(version));
            if (!removeTimestampedSnapshot(versionDirectoryPath, numberToKeep, keepDate))
            {
                continue;
            }

            logger.info("Generate snapshot versioning metadata for {}.", versionDirectoryPath);

            mavenMetadataManager.generateLastSnapshotVersioningMetadata(artifactGroupId,
                                                                    artifactId,
                                                                    versionDirectoryPath,
                                                                    version,
                                                                    true);
        }
    }

    private boolean removeTimestampedSnapshot(RepositoryPath basePath,
                                              int numberToKeep,
                                              Date keepDate)
            throws IOException,
                   XmlPullParserException
    {
        Metadata metadata = mavenMetadataManager.readMetadata(basePath);

        if (metadata == null || metadata.getVersioning() == null)
        {
            return false;
        }
        
        /**
         * map of snapshots for removing
         * k - number of the build, v - version of the snapshot
         */
        Map<Integer, String> mapToRemove = getRemovableTimestampedSnapshots(metadata, numberToKeep, keepDate);

        if (mapToRemove.isEmpty())
        {
            return false;
        }
        List<String> removingSnapshots = new ArrayList<>();

        new ArrayList<>(mapToRemove.values()).forEach(e -> removingSnapshots.add(metadata.getArtifactId()
                                                                                         .concat("-")
                                                                                         .concat(e)
                                                                                         .concat(".jar")));

        try (final DirectoryStream<Path> directoryStream = Files.newDirectoryStream(basePath))
        {
            for (Path path : directoryStream)
            {
                if (!Files.isRegularFile(path))
                {
                    continue;
                }
                
                RepositoryPath repositoryPath = (RepositoryPath) path;
                final String filename = path.getFileName().toString();
                
                if (!removingSnapshots.contains(filename) ||
                    !RepositoryFiles.isArtifact(repositoryPath) ||
                    RepositoryFiles.isMetadata(repositoryPath))
                {
                    continue;
                }
                
                try
                {
                    RepositoryFiles.delete(repositoryPath, true);

                    RepositoryPath pomRepositoryPath = repositoryPath.resolveSibling(filename.replace(".jar", ".pom"));
                    
                    RepositoryFiles.delete(pomRepositoryPath,true);
                }
                catch (IOException ex)
                {
                    logger.error(ex.getMessage(), ex);
                }
            }
        }
        return true;
    }

    /**
     * To get map of removable timestamped snapshots
     *
     * @param metadata     type Metadata
     * @param numberToKeep type int
     * @param keepDate     type Date
     * @return type Map<Integer, String>
     */
    private Map<Integer, String> getRemovableTimestampedSnapshots(Metadata metadata,
                                                                  int numberToKeep,
                                                                  Date keepDate)
    {
        /**
         * map of the snapshots in metadata file
         * k - number of the build, v - version of the snapshot
         */
        Map<Integer, SnapshotVersionDecomposition> snapshots = new HashMap<>();

        /**
         * map of snapshots for removing
         * k - number of the build, v - version of the snapshot
         */
        Map<Integer, String> mapToRemove = new HashMap<>();

        metadata.getVersioning()
                .getSnapshotVersions()
                .forEach(e ->
                         {
                             if ("jar".equals(e.getExtension()))
                             {
                                 SnapshotVersionDecomposition snapshotVersion = SnapshotVersionDecomposition.of(e.getVersion());
                                 if (SnapshotVersionDecomposition.INVALID.equals(snapshotVersion))
                                 {
                                     logger.warn("Received invalid snapshot version {}", snapshotVersion);
                                     return;
                                 }
                                 snapshots.put(snapshotVersion.getBuildNumber(), snapshotVersion);
                             }
                         });

        if (numberToKeep != 0 && snapshots.size() > numberToKeep)
        {
            snapshots.forEach((k, v) ->
                              {
                                  if (mapToRemove.size() < snapshots.size() - numberToKeep)
                                  {
                                      mapToRemove.put(k, v.getVersion());
                                  }
                              });
        }
        else if (numberToKeep == 0 && keepDate != null)
        {
            snapshots.forEach((k, v) ->
                              {
                                  try
                                  {
                                      DateFormat formatter = new SimpleDateFormat(TIMESTAMP_FORMAT);
                                      Date snapshotVersionDate = formatter.parse(v.getTimestamp());

                                      if (keepDate.after(snapshotVersionDate))
                                      {
                                          mapToRemove.put(k, v.getVersion());
                                      }
                                  }
                                  catch (ParseException e)
                                  {
                                      logger.error(e.getMessage(), e);
                                  }
                              });
        }

        return mapToRemove;
    }
}
