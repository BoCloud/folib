package com.veadan.folib.services.impl;

import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.domain.PubPackageMetadata;
import com.veadan.folib.domain.PubPackageVersionMetadata;
import com.veadan.folib.enums.PubIndexTypeEnum;
import com.veadan.folib.enums.PubRepositoryTypeEnum;
import com.veadan.folib.indexer.PubPackageMetadataIndexer;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.services.PubProvider;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.utils.PubUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.nio.file.Files;
import java.util.Objects;
import java.util.Optional;

/**
 * @author leipenghui
 **/
@Slf4j
@Component
public class PubHostedProvider implements PubProvider {

    @Inject
    private PubProviderRegistry pubProviderRegistry;

    @Inject
    private PubPackageMetadataIndexer pubPackageMetadataIndexer;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @PostConstruct
    @Override
    public void register() {
        pubProviderRegistry.addProvider(PubRepositoryTypeEnum.PUB_HOSTED.getType(), this);
        log.info("Registered pub provider '[{}]' with alias '[{}]'.",
                getClass().getCanonicalName(), PubRepositoryTypeEnum.PUB_HOSTED.getType());
    }

    @Override
    public PubPackageVersionMetadata inspectVersion(Repository repository, String packageName, String version, String targetUrl) {
        JSONObject pubPackageMetadataJson = packages(repository, packageName, null);
        if (Objects.nonNull(pubPackageMetadataJson)) {
            PubPackageMetadata pubPackageMetadata = pubPackageMetadataJson.toJavaObject(PubPackageMetadata.class);
            if (Objects.nonNull(pubPackageMetadata)) {
                log.debug("Attempting to find the version {} in package metadata {}", version, packageName);
                Optional<PubPackageVersionMetadata> pubSpecificVersionMetadata = pubPackageMetadata.getVersions().stream().filter(versionMetadata -> versionMetadata.getVersion().equals(version)).findFirst();
                if (pubSpecificVersionMetadata.isPresent()) {
                    log.debug("Attempting to transform metadata content and minimize data, version {} in the package {}", version, packageName);
                    return pubSpecificVersionMetadata.get();
                }
            }
        }
        return null;
    }

    @Override
    public JSONObject packages(Repository repository, String packageName, String targetUrl) {
        String packageMetadataFilePath = PubUtils.getPackageMetadataFilePath(packageName);
        RepositoryPath packageMetadataRepositoryPath = repositoryPathResolver.resolve(repository, packageMetadataFilePath);
        try {
            if (Objects.isNull(packageMetadataRepositoryPath) || !Files.exists(packageMetadataRepositoryPath) || RepositoryFiles.hasRefreshContent(packageMetadataRepositoryPath)) {
                RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository, packageName);
                if (!Files.exists(repositoryPath)) {
                    return null;
                }
                pubPackageMetadataIndexer.indexAsSystem(repositoryPath, PubIndexTypeEnum.REINDEX);
                packageMetadataRepositoryPath = repositoryPathResolver.resolve(repository, packageMetadataFilePath);
                if (Objects.isNull(packageMetadataRepositoryPath) || !Files.exists(packageMetadataRepositoryPath)) {
                    return null;
                }
            }
            return JSONObject.parseObject(Files.readString(packageMetadataRepositoryPath));
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
        return null;
    }

    @Override
    public JSONObject getLocalPackages(Repository repository, String packageName, String targetUrl) {
        return packages(repository, packageName, targetUrl);
    }

}
