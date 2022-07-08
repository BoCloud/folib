package com.veadan.folib.providers.layout;

import com.veadan.folib.providers.io.*;
import com.veadan.folib.providers.search.SearchException;
import com.veadan.folib.services.ArtifactSearchService;
import com.veadan.folib.storage.search.SearchRequest;
import com.veadan.folib.storage.search.SearchResult;
import com.veadan.folib.storage.search.SearchResults;
import com.veadan.folib.artifact.MavenArtifactUtils;
import com.veadan.folib.providers.io.*;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.metadata.MavenMetadataManager;
import com.veadan.folib.storage.metadata.MetadataHelper;
import com.veadan.folib.storage.metadata.MetadataType;
import com.veadan.folib.storage.repository.Repository;

import javax.inject.Inject;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.spi.FileSystemProvider;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import org.apache.commons.io.FilenameUtils;
import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.artifact.repository.metadata.Metadata;
import org.codehaus.plexus.util.xml.pull.XmlPullParserException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author xuxinping
 *
 */
public class Maven2FileSystemProvider extends LayoutFileSystemProvider
{

    private static final Logger logger = LoggerFactory.getLogger(Maven2FileSystemProvider.class);
    
    @Inject
    private Maven2LayoutProvider layoutProvider;

    @Inject
    private ArtifactSearchService artifactSearchService;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private MavenMetadataManager mavenMetadataManager;
    
    
    public Maven2FileSystemProvider(FileSystemProvider storageFileSystemProvider)
    {
        super(storageFileSystemProvider);
    }

    @Override
    protected AbstractLayoutProvider getLayoutProvider()
    {
        return layoutProvider;
    }

    @Override
    public void delete(Path path,
                       boolean force)
        throws IOException
    {
        RepositoryPath repositoryPath = (RepositoryPath) path;
        
        logger.debug("Removing {}...", repositoryPath);

        if (Files.isDirectory(repositoryPath))
        {
            cleanupDirectory(repositoryPath.relativize(), force);
        }

        super.delete(repositoryPath, force);
    }

    private void cleanupDirectory(RepositoryPath repositoryPathRelative,
                                  boolean force)
        throws IOException
    {
        Repository repository = repositoryPathRelative.getRepository();
        Storage storage = repository.getStorage();
        
        List<String> artifactCoordinateElements = StreamSupport.stream(repositoryPathRelative.spliterator(), false)
                                                               .map(p -> p.toString())
                                                               .collect(Collectors.toList());
        if (artifactCoordinateElements.size() < 2)
        {
            return;
        }
        
        StringBuffer groupId = new StringBuffer();
        for (int i = 0; i < artifactCoordinateElements.size() - 2; i++)
        {
            String element = artifactCoordinateElements.get(i);
            groupId.append((groupId.length() == 0) ? element : "." + element);
        }

        String artifactId = artifactCoordinateElements.get(artifactCoordinateElements.size() - 2);
        String version = artifactCoordinateElements.get(artifactCoordinateElements.size() - 1);

        RepositoryPath pomFilePath = repositoryPathRelative.resolve(artifactId + "-" + version + ".pom");

        // If there is a pom file, read it.
        if (Files.exists(pomFilePath))
        {
            // Run a search against the index and get a list of all the artifacts matching this exact GAV
            SearchRequest request = new SearchRequest(storage.getId(),
                                                      repository.getId(),
                                                      "+g:" + groupId + " " +
                                                      "+a:" + artifactId + " " +
                                                      "+v:" + version);

            try
            {
                SearchResults results = artifactSearchService.search(request);

                for (SearchResult result : results.getResults())
                {
                    delete(repositoryPathResolver.resolve(repository, result.getArtifactCoordinates()), force);
                }
            }
            catch (SearchException e)
            {
                logger.error(e.getMessage(), e);
            }
        }
        // Otherwise, this is either not an artifact directory, or not a valid Maven artifact
    }

    @Override
    public void deleteMetadata(RepositoryPath artifactPath)
    {
        try
        {
            RepositoryPath artifactBasePath = artifactPath;
            RepositoryPath artifactIdLevelPath;
            try
            {
                artifactIdLevelPath = artifactBasePath.getParent();
            }
            catch (RepositoryRelativePathConstructionException e)
            {
                //it's repository root directory, so we have nothing to clean here
                return;
            }

            if (Files.exists(artifactPath))
            {

                RepositoryFileAttributes artifactFileAttributes = Files.readAttributes(artifactPath,
                                                                                       RepositoryFileAttributes.class);

                if (!artifactFileAttributes.isDirectory())
                {
                    artifactBasePath = artifactBasePath.getParent();
                    artifactIdLevelPath = artifactIdLevelPath.getParent();

                    // This is at the version level
                    try (Stream<Path> pathStream = Files.list(artifactBasePath))
                    {
                        Path pomPath = pathStream.filter(p -> p.getFileName()
                                                 .toString()
                                                 .endsWith(".pom"))
                                                 .findFirst()
                                                 .orElse(null);

                        if (pomPath != null)
                        {
                            String version = MavenArtifactUtils
                                                     .convertPathToArtifact(RepositoryFiles.relativizePath(artifactPath))
                                                     .getVersion();
                            version = version == null ? pomPath.getParent().getFileName().toString() : version;

                            deleteMetadataAtVersionLevel(artifactBasePath, version);
                        }
                    }

                }
            }
            else
            {
                artifactBasePath = artifactBasePath.getParent();
                artifactIdLevelPath = artifactIdLevelPath.getParent();
            }

            if (Files.exists(artifactIdLevelPath))
            {
                // This is at the artifact level
                try (Stream<Path> pathStream = Files.list(artifactIdLevelPath))
                {
                    Path mavenMetadataPath = pathStream.filter(p -> p.getFileName()
                                                       .toString()
                                                       .endsWith("maven-metadata.xml"))
                                                       .findFirst()
                                                       .orElse(null);

                    if (mavenMetadataPath != null)
                    {
                        String version = FilenameUtils.getName(artifactBasePath.toString());

                        deleteMetadataAtArtifactLevel((RepositoryPath) mavenMetadataPath.getParent(), version);
                    }
                }
            }
        }
        catch (IOException | XmlPullParserException e)
        {
            // We won't do anything in this case because it doesn't have an impact to the deletion
            logger.error(e.getMessage(), e);
        }
    }

    public void deleteMetadataAtVersionLevel(RepositoryPath metadataBasePath,
                                             String version)
        throws IOException,
               XmlPullParserException
    {
        
        if (ArtifactUtils.isSnapshot(version) && Files.exists(metadataBasePath))
        {
            Metadata metadataVersionLevel = mavenMetadataManager.readMetadata(metadataBasePath);
            if (metadataVersionLevel != null && metadataVersionLevel.getVersioning() != null &&
                    metadataVersionLevel.getVersioning().getVersions().contains(version))
            {
                metadataVersionLevel.getVersioning().getVersions().remove(version);

                MetadataHelper.setLastUpdated(metadataVersionLevel.getVersioning());

                mavenMetadataManager.storeMetadata(metadataBasePath,
                                                   null,
                                                   metadataVersionLevel,
                                                   MetadataType.SNAPSHOT_VERSION_LEVEL);
            }
        }
    }

    public void deleteMetadataAtArtifactLevel(RepositoryPath artifactPath,
                                              String version)
        throws IOException,
               XmlPullParserException
    {

        Metadata metadataVersionLevel = mavenMetadataManager.readMetadata(artifactPath);
        if (metadataVersionLevel != null && metadataVersionLevel.getVersioning() != null)
        {
            metadataVersionLevel.getVersioning().getVersions().remove(version);

            if (version.equals(metadataVersionLevel.getVersioning().getLatest()))
            {
                MetadataHelper.setLatest(metadataVersionLevel);
            }

            if (version.equals(metadataVersionLevel.getVersioning().getRelease()))
            {
                MetadataHelper.setRelease(metadataVersionLevel);
            }

            MetadataHelper.setLastUpdated(metadataVersionLevel.getVersioning());

            mavenMetadataManager.storeMetadata(artifactPath,
                                               null,
                                               metadataVersionLevel,
                                               MetadataType.ARTIFACT_ROOT_LEVEL);
        }
    }
}
