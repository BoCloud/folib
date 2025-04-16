package com.veadan.folib.services;


import com.veadan.folib.index.model.RepoData;
import com.veadan.folib.index.model.RepoDataEventKind;
import com.veadan.folib.providers.io.RepositoryPath;
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
    public RepoData getRepoData(String repoKey);

    public RepoData getCurrentRepoData(String repoKey);

    public void sendRepoDataEvent(RepoDataEventKind kind, String repoKey, String artifactName);

    public boolean checkPackageExistsInRepoData(RepoData repoData,
                                                String artifactName);
}
