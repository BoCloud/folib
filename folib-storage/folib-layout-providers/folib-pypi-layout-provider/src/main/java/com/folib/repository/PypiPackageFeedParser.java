package com.folib.repository;

import com.folib.artifact.ArtifactTag;
import com.folib.artifact.coordinates.PypiArtifactCoordinates;
import com.folib.domain.Artifact;
import com.folib.domain.ArtifactEntity;
import com.folib.domain.ArtifactTagEntity;
import com.folib.domain.PypiPackageInfo;
import com.folib.pypi.PypiSearchResult;
import com.folib.repositories.ArtifactRepository;
import com.folib.services.ArtifactIdGroupService;
import com.folib.services.ArtifactTagService;
import com.folib.storage.repository.Repository;
import com.folib.util.LocalDateTimeInstance;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import javax.inject.Inject;
import java.time.LocalDateTime;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.regex.Pattern;

@Component
public class PypiPackageFeedParser {
    private static final Pattern PACKAGE_NAME_PATTERN = Pattern.compile(PypiPackageInfo.NAME_FORMAT);
    @Inject
    private ArtifactTagService artifactTagService;
    @Inject
    private ArtifactRepository artifactRepository;
    @Inject
    private ArtifactIdGroupService repositoryArtifactIdGroupService;

    @Transactional(rollbackFor = Exception.class, timeout = 10 * 60 * 60)
    public void parseSearchResult(Repository repository,
                                  List<PypiSearchResult> pypiSearchResult) {
        ArtifactTag lastVersionTag = artifactTagService.findOneOrCreate(ArtifactTagEntity.LAST_VERSION);
        String repositoryId = repository.getId();
        String storageId = repository.getStorage().getId();
        Set<Artifact> artifactToSaveSet = new HashSet<>();
        for (PypiSearchResult searchResult : pypiSearchResult) {
            ArtifactEntity remoteArtifactEntry = parseVersion(storageId, repositoryId, searchResult);
            remoteArtifactEntry.getTagSet().add(lastVersionTag);
            artifactToSaveSet.add(remoteArtifactEntry);
        }
        repositoryArtifactIdGroupService.saveArtifacts(repository, artifactToSaveSet);
    }

    private ArtifactEntity parseVersion(String storageId,
                                        String repositoryId,
                                        PypiSearchResult searchResult) {
        PypiArtifactCoordinates c = PypiArtifactCoordinates.parse(searchResult.getArtifactName());
        c.setPath(searchResult.getArtifactUrl());
        LocalDateTime now = LocalDateTimeInstance.now();

        Artifact artifact = artifactRepository.findOneArtifact(storageId, repositoryId, c.buildPath());
        ArtifactEntity remoteArtifactEntry;
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
