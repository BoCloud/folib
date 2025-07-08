package com.folib.services.impl;

import com.folib.enums.PypiIndexTypeEnum;
import com.folib.enums.PypiRepositoryTypeEnum;
import com.folib.indexer.PypiPackageMetadataIndexer;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.services.PypiProvider;
import com.folib.storage.repository.Repository;
import com.folib.util.PypiUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.nio.file.Files;
import java.util.Objects;

/**
 * @author veadan
 **/
@Slf4j
@Component
public class PypiHostedProvider implements PypiProvider {

    @Inject
    private PypiProviderRegistry pypiProviderRegistry;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private PypiPackageMetadataIndexer pypiPackageMetadataIndexer;

    @PostConstruct
    @Override
    public void register() {
        pypiProviderRegistry.addProvider(PypiRepositoryTypeEnum.PYPI_HOSTED.getType(), this);
        log.info("Registered pypi provider '[{}]' with alias '[{}]'.",
                getClass().getCanonicalName(), PypiRepositoryTypeEnum.PYPI_HOSTED.getType());
    }

    @Override
    public String packages(Repository repository, String packageName, String targetUrl) {
        String packageMetadataFilePath = PypiUtils.getPackageIndexPathLocalRepo(packageName);
        RepositoryPath packageMetadataRepositoryPath = repositoryPathResolver.resolve(repository, packageMetadataFilePath);
        try {
            if (Objects.isNull(packageMetadataRepositoryPath) || !Files.exists(packageMetadataRepositoryPath) || RepositoryFiles.hasRefreshContent(packageMetadataRepositoryPath)) {
                RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository, packageName);
//                if (!Files.exists(repositoryPath)) {
//                    return null;
//                }
                pypiPackageMetadataIndexer.indexAsSystem(repositoryPath, PypiIndexTypeEnum.REINDEX);
                packageMetadataRepositoryPath = repositoryPathResolver.resolve(repository, packageMetadataFilePath);
                if (Objects.isNull(packageMetadataRepositoryPath) || !Files.exists(packageMetadataRepositoryPath)) {
                    return null;
                }
            }
            return Files.readString(packageMetadataRepositoryPath);
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
        return null;
    }

    @Override
    public String getLocalPackages(Repository repository, String packageName, String targetUrl) {
        return packages(repository, packageName, targetUrl);
    }

}
