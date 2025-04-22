package com.veadan.folib.services;


import com.veadan.folib.event.CondaRepodataEvent;
import com.veadan.folib.index.model.RepoData;
import com.veadan.folib.index.model.RepoDataEventKind;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.storage.repository.Repository;
import lombok.NonNull;

/**
 * @author LingengMa
 * @date 2025/04/14 14:00
 * @Description:
 */

public interface CondaRepoDataService {
    /**
     * @repokey: {storageId}/{repositoryId}/{platform}
     */
    public RepoData getRepoData(Repository repository, String platformId);

    public RepoData getCurrentRepoData(Repository repository, String platformId);

    public void sendRepoDataEvent(CondaRepodataEvent event);

    public boolean checkPackageExistsInRepoData(RepoData repoData,
                                                String artifactName);

    public RepoData aggregateCondaGroupPlatformRepoData(Repository repository, String platformId);
}
