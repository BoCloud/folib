package com.veadan.folib.repository;

import com.veadan.folib.artifact.ArtifactTag;
import com.veadan.folib.artifact.coordinates.PhpArtifactCoordinates;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.ArtifactEntity;
import com.veadan.folib.domain.ArtifactTagEntity;
import com.veadan.folib.php.PhpSearchPackage;
import com.veadan.folib.php.PhpSearchResult;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.services.ArtifactIdGroupService;
import com.veadan.folib.services.ArtifactTagService;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.util.LocalDateTimeInstance;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import javax.inject.Inject;
import java.time.LocalDateTime;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

@Component
public class PhpPackageFeedParser {

    @Inject
    private ArtifactTagService artifactTagService;

    @Inject
    private ArtifactIdGroupService repositoryArtifactIdGroupService;

    @Inject
    private ArtifactRepository artifactRepository;

    @Transactional(rollbackFor = Exception.class)
    public void parseSearchResult(Repository repository,
                                  PhpSearchResult phpSearchResult) {
        ArtifactTag lastVersionTag = artifactTagService.findOneOrCreate(ArtifactTagEntity.LAST_VERSION);
        String repositoryId = repository.getId();
        String storageId = repository.getStorage().getId();
        Set<Artifact> artifactToSaveSet = new HashSet<>();
        for (PhpSearchPackage phpSearchPackage : phpSearchResult.getResults()) {
            ArtifactEntity remoteArtifactEntry = parseVersion(storageId, repositoryId, phpSearchPackage);
            remoteArtifactEntry.getTagSet().add(lastVersionTag);
            artifactToSaveSet.add(remoteArtifactEntry);
        }
        repositoryArtifactIdGroupService.saveArtifacts(repository, artifactToSaveSet);
    }

    private ArtifactEntity parseVersion(String storageId,
                                        String repositoryId,
                                        PhpSearchPackage phpSearchPackage) {
        PhpArtifactCoordinates c = new PhpArtifactCoordinates();
        c.setComposerVersion(PhpArtifactCoordinates.COMPOSER_P2.replace("/", ""));
        c.setName(phpSearchPackage.getName());
        c.setDescription(phpSearchPackage.getDescription());
        c.setExtension(PhpArtifactCoordinates.JSON);
        LocalDateTime now = LocalDateTimeInstance.now();

        Artifact artifact = artifactRepository.findOneArtifact(storageId, repositoryId, c.buildPath());
        ArtifactEntity remoteArtifactEntry = null;
        if (Objects.nonNull(artifact)) {
            //已存在
            remoteArtifactEntry = new ArtifactEntity(artifact.getNativeId(), storageId, repositoryId, artifact.getUuid(), c);
        } else {
            //不存在
            remoteArtifactEntry = new ArtifactEntity(storageId, repositoryId, c);
            remoteArtifactEntry.setStorageId(storageId);
            remoteArtifactEntry.setRepositoryId(repositoryId);
            remoteArtifactEntry.setArtifactCoordinates(c);
            remoteArtifactEntry.setLastUsed(now);
            remoteArtifactEntry.setLastUpdated(now);
            remoteArtifactEntry.setDownloadCount(0);
            remoteArtifactEntry.setArtifactFileExists(Boolean.FALSE);
        }
        return remoteArtifactEntry;
    }

}
