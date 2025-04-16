package com.veadan.folib.services;

import com.veadan.folib.providers.io.RepositoryPath;
import lombok.NonNull;

/**
 * @author LingengMa
 * @date 2025/04/16 10:29
 * @Description:
 */

public interface CondaArtifactService {
    public boolean checkArtifactExist(@NonNull RepositoryPath path) throws Exception;

    public void unpublishPackage(@NonNull RepositoryPath path) throws Exception;

}
