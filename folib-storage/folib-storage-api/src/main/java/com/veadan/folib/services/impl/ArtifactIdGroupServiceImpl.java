package com.veadan.folib.services.impl;

import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.locks.Lock;
import java.util.stream.Collectors;

import javax.inject.Inject;

import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.providers.io.RepositoryPathLock;
import com.veadan.folib.services.ArtifactIdGroupService;
import com.veadan.folib.services.ArtifactTagService;
import com.veadan.folib.artifact.ArtifactTag;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.ArtifactIdGroup;
import com.veadan.folib.domain.ArtifactIdGroupEntity;
import com.veadan.folib.domain.ArtifactTagEntity;
import com.veadan.folib.repositories.ArtifactIdGroupRepository;
import com.veadan.folib.storage.repository.Repository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.Assert;

import com.google.common.collect.Iterables;

/**
 * @author Przemyslaw Fusik
 * @author sbespalov
 */
@Service
@Transactional
public class ArtifactIdGroupServiceImpl
        extends AbstractArtifactGroupService<ArtifactIdGroup>
        implements ArtifactIdGroupService
{

    private static final Logger logger = LoggerFactory.getLogger(ArtifactIdGroupEntity.class);

    @Inject
    private ArtifactTagService artifactTagService;

    @Inject
    private ArtifactIdGroupRepository artifactIdGroupRepository;

    @Inject
    private RepositoryPathLock repositoryPathLock;

    @Override
    public void saveArtifacts(Repository repository,
                              Set<Artifact> artifactToSaveSet)
    {
        Map<String, List<Artifact>> artifactByGroupIdMap = artifactToSaveSet.stream()
                                                                            .collect(Collectors.groupingBy(a -> a.getArtifactCoordinates()
                                                                                                                 .getId()));
        for (Entry<String, List<Artifact>> artifactIdGroupEntry : artifactByGroupIdMap.entrySet())
        {
            List<Artifact> artifacts = artifactIdGroupEntry.getValue();
            String artifactGroupId = artifactIdGroupEntry.getKey();
            ArtifactTag lastVersionTag = artifactTagService.findOneOrCreate(ArtifactTagEntity.LAST_VERSION);
            for (List<Artifact> artifacstBatch : Iterables.partition(artifacts, 50))
            {
                Lock lock = repositoryPathLock.lock(artifactGroupId).writeLock();
                lock.lock();
                try
                {
                    ArtifactIdGroup artifactGroup = artifactIdGroupRepository.findArtifactsGroupWithTag(repository.getStorage()
                                                                                                                  .getId(),
                                                                                                        repository.getId(),
                                                                                                        artifactGroupId,
                                                                                                        Optional.of(lastVersionTag))
                                                                             .orElseGet(() -> create(repository.getStorage().getId(),
                                                                                                     repository.getId(),
                                                                                                     artifactGroupId));

                    ArtifactCoordinates lastVersion = addArtifactsToGroup(artifacstBatch, artifactGroup);
                    logger.debug("Last version for group [{}] is [{}] with [{}]",
                                 artifactGroup.getName(),
                                 lastVersion.getVersion(),
                                 lastVersion.getPath());

                    artifactIdGroupRepository.merge(artifactGroup);
                }
                finally
                {
                    lock.unlock();
                }
            }
        }
    }

    private ArtifactCoordinates addArtifactsToGroup(List<Artifact> artifacts,
                                                    ArtifactIdGroup artifactGroup)
    {
        ArtifactCoordinates lastVersion = null;
        for (Artifact artifact : artifacts)
        {
            lastVersion = addArtifactToGroup(artifactGroup, artifact);
        }
        return lastVersion;
    }

    @Override
    public ArtifactCoordinates addArtifactToGroup(ArtifactIdGroup artifactGroup,
                                                  Artifact artifact)
    {
        ArtifactCoordinates coordinates = artifact.getArtifactCoordinates();
        Assert.notNull(coordinates, "coordinates should not be null");

        ArtifactTag lastVersionTag = artifactTagService.findOneOrCreate(ArtifactTagEntity.LAST_VERSION);

        artifact.getTagSet().add(lastVersionTag);
        artifactGroup.addArtifact(artifact);
        
        Artifact lastVersionArtifact = artifactGroup.getArtifacts()
                                                    .stream()
                                                    .filter(e -> e.getTagSet().contains(lastVersionTag))
                                                    .reduce((a1,
                                                             a2) -> reduceByLastVersionTag(a1, a2, lastVersionTag))
                                                    .get();

        return lastVersionArtifact.getArtifactCoordinates();
    }

    private Artifact reduceByLastVersionTag(Artifact a1,
                                            Artifact a2,
                                            ArtifactTag lastVersionTag)
    {
        int artifactCoordinatesComparison = a1.getArtifactCoordinates()
                                              .compareTo(a2.getArtifactCoordinates());
        if (artifactCoordinatesComparison > 0)
        {
            a2.getTagSet().remove(lastVersionTag);
            return a1;
        }
        else if (artifactCoordinatesComparison < 0)
        {
            a1.getTagSet().remove(lastVersionTag);
            return a2;
        }

        return a1;
    }

    protected ArtifactIdGroup create(String storageId,
                                     String repositoryId,
                                     String artifactId)
    {
        return new ArtifactIdGroupEntity(storageId, repositoryId, artifactId);
    }

}
