package com.folib.util;

import com.folib.domain.DebianPackagesContext;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.services.ArtifactManagementService;
import com.folib.storage.repository.Repository;

import java.io.File;
import java.io.InputStream;

/**
 * @author veadan
 * @since 2024-09-02 16:26
 */
public interface DebianFormatWriter {

    void writePackagesIndexFile(Repository repo, DebianPackagesContext packagesContext, File packagesFile, RepositoryPathResolver resolver,ArtifactManagementService artifactManagement) throws RuntimeException;

    InputStream createPackagesIndexInputStream(String packagesFilePlainTextContent);
}
