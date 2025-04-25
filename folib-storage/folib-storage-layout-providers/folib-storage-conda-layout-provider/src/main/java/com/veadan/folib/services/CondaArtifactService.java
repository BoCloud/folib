package com.veadan.folib.services;

import com.veadan.folib.index.model.Index;
import com.veadan.folib.index.model.RepoDataPackage;
import com.veadan.folib.providers.io.RepositoryPath;
import lombok.NonNull;
import org.springframework.stereotype.Component;

/**
 * @author LingengMa
 * @date 2025/04/16 10:29
 * @Description:
 */

public interface CondaArtifactService {
    public boolean checkArtifactExist(@NonNull RepositoryPath path) throws Exception;

    public void reIndexArtifact(@NonNull RepositoryPath path) throws Exception;

    public void unpublishPackage(@NonNull RepositoryPath path) throws Exception;

    public Index extract(@NonNull String repoKey, @NonNull String artifactName);

    public RepoDataPackage getRepoDataPackage(@NonNull RepositoryPath path);

}
