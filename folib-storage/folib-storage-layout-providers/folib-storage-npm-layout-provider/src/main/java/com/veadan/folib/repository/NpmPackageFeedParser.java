package com.veadan.folib.repository;

import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.artifact.ArtifactTag;
import com.veadan.folib.artifact.coordinates.NpmArtifactCoordinates;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.ArtifactEntity;
import com.veadan.folib.domain.ArtifactTagEntity;
import com.veadan.folib.npm.metadata.*;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.services.ArtifactIdGroupService;
import com.veadan.folib.services.ArtifactTagService;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.util.LocalDateTimeInstance;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.MapUtils;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.time.LocalDateTime;
import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

@Slf4j
@Component
public class NpmPackageFeedParser {

    @Inject
    private ArtifactTagService artifactTagService;

    @Inject
    private ArtifactIdGroupService repositoryArtifactIdGroupService;

    @Inject
    private ArtifactRepository artifactRepository;

    public void parseSearchResult(Repository repository,
                                  SearchResults searchResults) {
        ArtifactTag lastVersionTag = artifactTagService.findOneOrCreate(ArtifactTagEntity.LAST_VERSION);

        String repositoryId = repository.getId();
        String storageId = repository.getStorage().getId();

        Set<Artifact> artifactToSaveSet = new HashSet<>();
        for (SearchResult searchResult : searchResults.getObjects()) {
            PackageEntry packageEntry = searchResult.getPackage();

            ArtifactEntity remoteArtifactEntry = parseVersion(storageId, repositoryId, packageEntry);
            if (remoteArtifactEntry == null) {
                continue;
            }

            remoteArtifactEntry.getTagSet().add(lastVersionTag);

            artifactToSaveSet.add(remoteArtifactEntry);
        }

        repositoryArtifactIdGroupService.saveArtifacts(repository, artifactToSaveSet);

    }

    public void parseFeed(Repository repository,
                          PackageFeed packageFeed) {
        if (packageFeed == null) {
            return;
        }

        String repositoryId = repository.getId();
        String storageId = repository.getStorage().getId();

        ArtifactTag lastVersionTag = artifactTagService.findOneOrCreate(ArtifactTagEntity.LAST_VERSION);

        Versions versions = packageFeed.getVersions();
        if (versions == null) {
            return;
        }

        Map<String, PackageVersion> versionMap = versions.getAdditionalProperties();
        if (versionMap == null || versionMap.isEmpty()) {
            return;
        }

        Set<Artifact> artifactToSaveSet = new HashSet<>();
        for (PackageVersion packageVersion : versionMap.values()) {
            log.info("storageId [{}] repositoryId [{}] name [{}] packageVersion [{}]", storageId, repositoryId, packageFeed.getName(), packageVersion);
            ArtifactEntity remoteArtifactEntry = parseVersion(storageId, repositoryId, packageVersion);
            if (remoteArtifactEntry == null) {
                continue;
            }

            if (packageVersion.getVersion().equals(packageFeed.getDistTags().getLatest())) {
                remoteArtifactEntry.getTagSet().add(lastVersionTag);
            }

            artifactToSaveSet.add(remoteArtifactEntry);
        }
        repositoryArtifactIdGroupService.saveArtifacts(repository, artifactToSaveSet);
    }

    private ArtifactEntity parseVersion(String storageId,
                                        String repositoryId,
                                        PackageVersion packageVersion) {
        NpmArtifactCoordinates c = NpmArtifactCoordinates.of(packageVersion.getName(), packageVersion.getVersion());
        log.info("storageId [{}] repositoryId [{}] name [{}] npmArtifactCoordinates [{}]", storageId, repositoryId, packageVersion.getName(), c);
        LocalDateTime now = LocalDateTimeInstance.now();
        Artifact artifact = artifactRepository.findOneArtifact(storageId, repositoryId, c.buildPath());
        ArtifactEntity remoteArtifactEntry = null;
        if (Objects.nonNull(artifact)) {
            //已存在
            remoteArtifactEntry = new ArtifactEntity(artifact.getNativeId(), storageId, repositoryId, artifact.getUuid(), c);
        } else {
            //不存在
            log.info("不存在 storageId [{}] repositoryId [{}] name [{}] npmArtifactCoordinates [{}]", storageId, repositoryId, packageVersion.getName(), c);
            remoteArtifactEntry = new ArtifactEntity(storageId, repositoryId, c);
            remoteArtifactEntry.setStorageId(storageId);
            remoteArtifactEntry.setRepositoryId(repositoryId);
            remoteArtifactEntry.setArtifactCoordinates(c);
            remoteArtifactEntry.setDownloadCount(0);
            remoteArtifactEntry.setArtifactFileExists(Boolean.FALSE);
        }
        Dependency dependency = packageVersion.getDependencies();
        if (Objects.nonNull(dependency) && MapUtils.isNotEmpty(dependency.getAdditionalProperties())) {
            remoteArtifactEntry.setDependencies(JSONObject.toJSONString(packageVersion.getDependencies().getAdditionalProperties()));
        }
        //make HEAD request for `tarball` URL ???
        // remoteArtifactEntry.setSizeInBytes(packageVersion.getProperties().getPackageSize());
        return remoteArtifactEntry;
    }

    private ArtifactEntity parseVersion(String storageId,
                                        String repositoryId,
                                        PackageEntry packageEntry) {
        String scope = packageEntry.getScope();
        String packageId = NpmArtifactCoordinates.calculatePackageId("unscoped".equals(scope) ? null : scope,
                packageEntry.getName());

        NpmArtifactCoordinates c = NpmArtifactCoordinates.of(packageId, packageEntry.getVersion());

        LocalDateTime now = LocalDateTimeInstance.now();
        Artifact artifact = artifactRepository.findOneArtifact(storageId, repositoryId, c.buildPath());
        ArtifactEntity remoteArtifactEntry = null;
        if (Objects.nonNull(artifact)) {
            //已存在
            remoteArtifactEntry = new ArtifactEntity(artifact.getNativeId(), storageId, repositoryId, artifact.getUuid(), c);
        } else {
            //不存在
            remoteArtifactEntry.setStorageId(storageId);
            remoteArtifactEntry.setRepositoryId(repositoryId);
            remoteArtifactEntry.setArtifactCoordinates(c);
            remoteArtifactEntry.setDownloadCount(0);
            remoteArtifactEntry.setArtifactFileExists(Boolean.FALSE);
        }
        return remoteArtifactEntry;
    }

}
