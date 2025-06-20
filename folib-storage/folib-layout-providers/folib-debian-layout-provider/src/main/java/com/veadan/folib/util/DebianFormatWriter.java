package com.veadan.folib.util;

import com.veadan.folib.domain.DebianPackagesContext;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.storage.repository.Repository;

import java.io.File;
import java.io.InputStream;

/**
 * @author huayanjun
 * @since 2024-09-02 16:26
 */
public interface DebianFormatWriter {

    void writePackagesIndexFile(Repository repo, DebianPackagesContext packagesContext, File packagesFile, RepositoryPathResolver resolver,ArtifactManagementService artifactManagement) throws RuntimeException;

    InputStream createPackagesIndexInputStream(String packagesFilePlainTextContent);
}
