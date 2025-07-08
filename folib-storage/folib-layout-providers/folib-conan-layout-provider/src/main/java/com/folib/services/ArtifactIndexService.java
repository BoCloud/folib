package com.folib.services;

import com.folib.providers.io.RepositoryPath;

/**
 * @author veadan
 * @date 2024/3/20
 **/
public interface ArtifactIndexService {


    /**
     * 重建index.json
     *
     * @param storageId    storageId
     * @param repositoryId repositoryId
     * @param artifactPath artifactPath
     */
    void rebuildIndex(String storageId, String repositoryId, String artifactPath);

    /**
     * 重建index.json
     *
     * @param repositoryPath repositoryPath
     */
    void rebuildIndex(RepositoryPath repositoryPath);
}
