package com.folib.storage.indexing.local;

import java.io.IOException;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import jakarta.inject.Inject;

import com.folib.artifact.coordinates.MavenArtifactCoordinates;
import com.folib.domain.Artifact;
import com.folib.domain.ArtifactIdGroup;
import com.folib.providers.io.RepositoryPath;
import com.folib.repositories.ArtifactIdGroupRepository;
import org.apache.maven.index.ArtifactContext;
import com.folib.storage.indexing.AbstractRepositoryIndexCreator;
import com.folib.storage.indexing.IndexPacker;
import com.folib.storage.indexing.IndexTypeEnum;
import com.folib.storage.indexing.Indexer;
import com.folib.storage.indexing.RepositoryCloseableIndexingContext;
import com.folib.storage.indexing.RepositoryIndexCreator.RepositoryIndexCreatorQualifier;
import com.folib.storage.indexing.RepositoryIndexDirectoryPathResolver;
import com.folib.storage.indexing.RepositoryIndexDirectoryPathResolver.RepositoryIndexDirectoryPathResolverQualifier;
import com.folib.storage.indexing.RepositoryIndexingContextFactory;
import com.folib.storage.indexing.RepositoryIndexingContextFactory.RepositoryIndexingContextFactoryQualifier;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryTypeEnum;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
@RepositoryIndexCreatorQualifier(RepositoryTypeEnum.HOSTED)
public class RepositoryHostedIndexCreator
        extends AbstractRepositoryIndexCreator
{

    private static final int REPOSITORY_ARTIFACT_GROUP_FETCH_PAGE_SIZE = 100;

    @Inject
    private ArtifactIdGroupRepository artifactIdGroupRepository;

    @Inject
    @RepositoryIndexDirectoryPathResolverQualifier(IndexTypeEnum.LOCAL)
    private RepositoryIndexDirectoryPathResolver indexDirectoryPathResolver;

    @Inject
    @RepositoryIndexingContextFactoryQualifier(IndexTypeEnum.LOCAL)
    private RepositoryIndexingContextFactory indexingContextFactory;

    @Override
    protected void onIndexingContextCreated(final RepositoryPath repositoryIndexDirectoryPath,
                                            final RepositoryCloseableIndexingContext indexingContext)
        throws IOException
    {
        indexingContext.purge();
        fulfillIndexingContext(indexingContext);
        IndexPacker.pack(repositoryIndexDirectoryPath, indexingContext);
    }

    @Override
    protected RepositoryIndexingContextFactory getRepositoryIndexingContextFactory()
    {
        return indexingContextFactory;
    }

    @Override
    protected RepositoryIndexDirectoryPathResolver getRepositoryIndexDirectoryPathResolver()
    {
        return indexDirectoryPathResolver;
    }

    private void fulfillIndexingContext(final RepositoryCloseableIndexingContext indexingContext)
        throws IOException
    {

        final Repository repository = indexingContext.getRepositoryRaw();
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();

        PageRequest pageRequest = PageRequest.of(0,
                                                 REPOSITORY_ARTIFACT_GROUP_FETCH_PAGE_SIZE);
        for (Page<ArtifactIdGroup> page = getPage(storageId,
                                                  repositoryId,
                                                  pageRequest); !page.isEmpty(); page = getPage(storageId, repositoryId,
                                                                                                pageRequest.next()))
        {
            List<ArtifactContext> artifactContexts = createArtifactContexts(page.getContent());
            Indexer.INSTANCE.addArtifactsToIndex(artifactContexts, indexingContext);
        }
    }

    private Page<ArtifactIdGroup> getPage(final String storageId,
                                          final String repositoryId,
                                          Pageable pageRequest)
    {
        return artifactIdGroupRepository.findMatching(storageId,
                                                      repositoryId,
                                                      pageRequest);
    }

    private List<ArtifactContext> createArtifactContexts(final List<ArtifactIdGroup> repositoryArtifactIdGroupEntries)
    {
        final List<ArtifactContext> artifactContexts = new ArrayList<>();
        for (final ArtifactIdGroup repositoryArtifactIdGroupEntry : repositoryArtifactIdGroupEntries)
        {
            final Map<String, List<Artifact>> groupedByVersion = groupArtifactEntriesByVersion(repositoryArtifactIdGroupEntry);
            for (final Map.Entry<String, List<Artifact>> sameVersionArtifactEntries : groupedByVersion.entrySet())
            {
                for (final Artifact artifactEntry : sameVersionArtifactEntries.getValue())
                {
                    if (!isIndexable(artifactEntry))
                    {
                        continue;
                    }

                    final List<Artifact> groupClone = new ArrayList<>(sameVersionArtifactEntries.getValue());
                    groupClone.remove(artifactEntry);

                    final ArtifactEntryArtifactContextHelper artifactContextHelper = createArtifactContextHelper(artifactEntry,
                                                                                                                 groupClone);
                    final ArtifactEntryArtifactContext ac = new ArtifactEntryArtifactContext(artifactEntry,
                            artifactContextHelper);
                    artifactContexts.add(ac);
                }
            }
        }
        return artifactContexts;
    }

    private Map<String, List<Artifact>> groupArtifactEntriesByVersion(final ArtifactIdGroup groupEntry)
    {
        final Map<String, List<Artifact>> groupedByVersion = new LinkedHashMap<>();
        for (final Artifact artifactEntry : groupEntry.getArtifacts())
        {
            final MavenArtifactCoordinates coordinates = (MavenArtifactCoordinates) artifactEntry.getArtifactCoordinates();
            final String version = coordinates.getVersion();
            List<Artifact> artifactEntries = groupedByVersion.get(version);
            if (artifactEntries == null)
            {
                artifactEntries = new ArrayList<>();
                groupedByVersion.put(version, artifactEntries);
            }
            artifactEntries.add(artifactEntry);
        }
        return groupedByVersion;
    }

    private ArtifactEntryArtifactContextHelper createArtifactContextHelper(final Artifact artifactEntry,
                                                                           final List<Artifact> group)
    {
        boolean pomExists = false;
        boolean sourcesExists = false;
        boolean javadocExists = false;
        if (group.size() < 1)
        {
            return new ArtifactEntryArtifactContextHelper(pomExists, sourcesExists, javadocExists);
        }
        final MavenArtifactCoordinates coordinates = (MavenArtifactCoordinates) artifactEntry.getArtifactCoordinates();
        if ("javadoc".equals(coordinates.getClassifier()) || "sources".equals(coordinates.getClassifier()))
        {
            return new ArtifactEntryArtifactContextHelper(pomExists, sourcesExists, javadocExists);
        }
        if ("pom".equals(coordinates.getExtension()))
        {
            return new ArtifactEntryArtifactContextHelper(pomExists, sourcesExists, javadocExists);
        }

        for (final Artifact neighbour : group)
        {
            final MavenArtifactCoordinates neighbourCoordinates = (MavenArtifactCoordinates) neighbour.getArtifactCoordinates();
            pomExists |= ("pom".equals(neighbourCoordinates.getExtension()) && neighbourCoordinates.getClassifier() == null);
            if (Objects.equals(coordinates.getExtension(), neighbourCoordinates.getExtension()))
            {
                javadocExists |= "javadoc".equals(neighbourCoordinates.getClassifier());
                sourcesExists |= "sources".equals(neighbourCoordinates.getClassifier());
            }
        }
        return new ArtifactEntryArtifactContextHelper(pomExists, sourcesExists, javadocExists);
    }

    /**
     * org.apache.maven.index.DefaultArtifactContextProducer#isIndexable(java.io.File)
     */
    private boolean isIndexable(final Artifact artifactEntry)
    {
        final String filename = Paths.get(artifactEntry.getArtifactPath()).getFileName().toString();

        if (filename.equals("maven-metadata.xml")
            // || filename.endsWith( "-javadoc.jar" )
            // || filename.endsWith( "-javadocs.jar" )
            // || filename.endsWith( "-sources.jar" )
            || filename.endsWith(".properties")
            // || filename.endsWith( ".xml" ) // NEXUS-3029
            || filename.endsWith(".asc") || filename.endsWith(".md5") || filename.endsWith(".sha1") || filename.endsWith(".sha256"))
        {
            return false;
        }

        return true;
    }
}
