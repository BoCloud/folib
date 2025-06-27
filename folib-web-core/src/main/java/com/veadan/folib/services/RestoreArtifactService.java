package com.veadan.folib.services;

import com.veadan.folib.providers.io.RepositoryPath;

/**
 * @author veadan
 * @date 2025/3/25
 **/
public interface RestoreArtifactService {

    /**
     * 还原回收站内制品
     * @param repositoryPath 制品信息
     * @throws Exception 异常
     */
    void restoreArtifact(RepositoryPath repositoryPath) throws Exception;
}
