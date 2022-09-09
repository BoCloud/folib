package com.veadan.folib.repository;

import com.veadan.folib.artifact.ArtifactTag;
import com.veadan.folib.artifact.coordinates.RpmArtifactCoordinates;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.ArtifactEntity;
import com.veadan.folib.domain.ArtifactTagEntity;
import com.veadan.folib.npm.metadata.*;
import com.veadan.folib.services.ArtifactIdGroupService;
import com.veadan.folib.services.ArtifactTagService;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.util.LocalDateTimeInstance;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import javax.inject.Inject;
import java.time.LocalDateTime;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

@Component
public class RpmPackageFeedParser {

    @Inject
    private ArtifactTagService artifactTagService;

    @Inject
    private ArtifactIdGroupService repositoryArtifactIdGroupService;


    @Transactional
    public void parseSearchResult(Repository repository,
                                  SearchResults searchResults)
    {
        ArtifactTag lastVersionTag = artifactTagService.findOneOrCreate(ArtifactTagEntity.LAST_VERSION);

        String repositoryId = repository.getId();
        String storageId = repository.getStorage().getId();

        Set<Artifact> artifactToSaveSet = new HashSet<>();
        for (SearchResult searchResult : searchResults.getObjects())
        {
            PackageEntry packageEntry = searchResult.getPackage();

            ArtifactEntity remoteArtifactEntry = parseVersion(storageId, repositoryId, packageEntry);
            if (remoteArtifactEntry == null)
            {
                continue;
            }

            remoteArtifactEntry.getTagSet().add(lastVersionTag);

            artifactToSaveSet.add(remoteArtifactEntry);
        }

        repositoryArtifactIdGroupService.saveArtifacts(repository, artifactToSaveSet);
    }

    @Transactional
    public void parseFeed(Repository repository,
                          PackageFeed packageFeed)
    {
        if (packageFeed == null)
        {
            return;
        }

        String repositoryId = repository.getId();
        String storageId = repository.getStorage().getId();

        ArtifactTag lastVersionTag = artifactTagService.findOneOrCreate(ArtifactTagEntity.LAST_VERSION);

        Versions versions = packageFeed.getVersions();
        if (versions == null)
        {
            return;
        }

        Map<String, PackageVersion> versionMap = versions.getAdditionalProperties();
        if (versionMap == null || versionMap.isEmpty())
        {
            return;
        }

        Set<Artifact> artifactToSaveSet = new HashSet<>();
        for (PackageVersion packageVersion : versionMap.values())
        {
            ArtifactEntity remoteArtifactEntry = parseVersion(storageId, repositoryId, packageVersion);
            if (remoteArtifactEntry == null)
            {
                continue;
            }

            if (packageVersion.getVersion().equals(packageFeed.getDistTags().getLatest()))
            {
                remoteArtifactEntry.getTagSet().add(lastVersionTag);
            }

            artifactToSaveSet.add(remoteArtifactEntry);
        }

        repositoryArtifactIdGroupService.saveArtifacts(repository, artifactToSaveSet);
    }

    private ArtifactEntity parseVersion(String storageId,
                                        String repositoryId,
                                        PackageVersion packageVersion)
    {
        RpmArtifactCoordinates c = RpmArtifactCoordinates.of(packageVersion.getName(), packageVersion.getVersion());

        LocalDateTime now = LocalDateTimeInstance.now();

        ArtifactEntity remoteArtifactEntry = new ArtifactEntity(storageId, repositoryId, c);
        remoteArtifactEntry.setStorageId(storageId);
        remoteArtifactEntry.setRepositoryId(repositoryId);
        remoteArtifactEntry.setArtifactCoordinates(c);
        remoteArtifactEntry.setLastUsed(now);
        remoteArtifactEntry.setLastUpdated(now);
        remoteArtifactEntry.setDownloadCount(0);
        remoteArtifactEntry.setArtifactFileExists(Boolean.FALSE);

        // TODO make HEAD request for `tarball` URL ???
        // remoteArtifactEntry.setSizeInBytes(packageVersion.getProperties().getPackageSize());

        return remoteArtifactEntry;
    }

    private ArtifactEntity parseVersion(String storageId,
                                        String repositoryId,
                                        PackageEntry packageEntry)
    {
        String scope = packageEntry.getScope();
        String packageId = RpmArtifactCoordinates.calculatePackageId("unscoped".equals(scope) ? null : scope,
                packageEntry.getName());

        RpmArtifactCoordinates c = RpmArtifactCoordinates.of(packageId, packageEntry.getVersion());

        LocalDateTime now = LocalDateTimeInstance.now();

        ArtifactEntity remoteArtifactEntry = new ArtifactEntity(storageId, repositoryId, c);
        remoteArtifactEntry.setStorageId(storageId);
        remoteArtifactEntry.setRepositoryId(repositoryId);
        remoteArtifactEntry.setArtifactCoordinates(c);
        remoteArtifactEntry.setLastUsed(now);
        remoteArtifactEntry.setLastUpdated(now);
        remoteArtifactEntry.setDownloadCount(0);
        remoteArtifactEntry.setArtifactFileExists(Boolean.FALSE);

        return remoteArtifactEntry;
    }

}
