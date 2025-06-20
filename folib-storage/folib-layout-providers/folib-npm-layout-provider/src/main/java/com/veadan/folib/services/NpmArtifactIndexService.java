package com.veadan.folib.services;

import com.veadan.folib.providers.io.RepositoryPath;

/**
 * @author leipenghui
 **/
public interface NpmArtifactIndexService {


    /**
     * 重建package.json
     *
     * @param storageId    storageId
     * @param repositoryId repositoryId
     * @param artifactPath artifactPath
     */
    void rebuildIndex(String storageId, String repositoryId, String artifactPath);

    /**
     * 重建package.json
     *
     * @param repositoryPath repositoryPath
     */
    void rebuildIndex(RepositoryPath repositoryPath);
}
