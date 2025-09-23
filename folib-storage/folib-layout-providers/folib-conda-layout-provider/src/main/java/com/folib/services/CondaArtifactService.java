package com.folib.services;

import com.folib.index.model.Index;
import com.folib.index.model.RepoDataPackage;
import com.folib.providers.io.RepositoryPath;
import lombok.NonNull;


public interface CondaArtifactService {
    public boolean checkArtifactExist(@NonNull RepositoryPath path) throws Exception;

    public void reIndexArtifact(@NonNull RepositoryPath path) throws Exception;

    public void unpublishPackage(@NonNull RepositoryPath path) throws Exception;

    public Index extract(@NonNull String repoKey, @NonNull String artifactName);

    public RepoDataPackage getRepoDataPackage(@NonNull RepositoryPath path);

    public RepoDataPackage convertIndexToRepoDataPackage(@NonNull RepositoryPath path, @NonNull Index index);

}
