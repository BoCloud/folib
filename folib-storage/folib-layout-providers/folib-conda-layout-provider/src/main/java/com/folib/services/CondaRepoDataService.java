package com.folib.services;


import com.folib.event.CondaRepodataEvent;
import com.folib.index.model.RepoData;
import com.folib.storage.repository.Repository;


public interface CondaRepoDataService {
    /**
     * @repokey: {storageId}/{repositoryId}/{platform}
     */
    public RepoData getRepoData(Repository repository, String platformId);

    public RepoData getCurrentRepoData(Repository repository, String platformId);

    public void sendRepoDataEvent(CondaRepodataEvent event);

    public boolean checkPackageExistsInRepoData(RepoData repoData, String artifactName);

    public RepoData aggregateCondaGroupPlatformRepoData(Repository repository, String platformId);

    public void reindexRepository(Repository repository);
}
