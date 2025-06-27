package com.veadan.folib.services.impl;

import com.alibaba.fastjson.JSONArray;
import com.veadan.folib.components.NpmComponent;
import com.veadan.folib.enums.NpmIndexTypeEnum;
import com.veadan.folib.enums.NpmRepositoryTypeEnum;
import com.veadan.folib.indexer.NpmPackageMetadataIndexer;
import com.veadan.folib.npm.metadata.PackageFeed;
import com.veadan.folib.npm.metadata.PackageVersion;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.services.NpmProvider;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.utils.NpmUtils;
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
public class NpmHostedProvider implements NpmProvider {

    @Inject
    private NpmProviderRegistry npmProviderRegistry;

    @Inject
    private NpmPackageMetadataIndexer npmPackageMetadataIndexer;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private NpmComponent npmComponent;

    @PostConstruct
    @Override
    public void register() {
        npmProviderRegistry.addProvider(NpmRepositoryTypeEnum.NPM_HOSTED.getType(), this);
        log.info("Registered npm provider '[{}]' with alias '[{}]'.",
                getClass().getCanonicalName(), NpmRepositoryTypeEnum.NPM_HOSTED.getType());
    }

    @Override
    public PackageVersion packageVersion(Repository repository, String packageName, String version, String targetUrl) {
        PackageFeed packageFeed = packageFeed(repository, packageName, targetUrl);
        if (Objects.nonNull(packageFeed)) {
            log.debug("Attempting to find the version {} in package metadata {}", version, packageName);
            PackageVersion packageVersion = packageFeed.getVersions().getAdditionalProperties().get(version);
            if (Objects.nonNull(packageVersion)) {
                log.debug("Attempting to transform metadata content and minimize data, version {} in the package {}", version, packageName);
                return packageVersion;
            }
        }
        return null;
    }

    @Override
    public PackageVersion getLocalPackageVersion(Repository repository, String packageName, String version, String targetUrl) {
        return packageVersion(repository, packageName, version, targetUrl);
    }

    @Override
    public PackageFeed packageFeed(Repository repository, String packageName, String targetUrl) {
        String packageFeedFilePath = NpmUtils.getPackageMetadataPath(packageName);
        RepositoryPath packageMetadataRepositoryPath = repositoryPathResolver.resolve(repository, packageFeedFilePath);
        try {
            if (Objects.isNull(packageMetadataRepositoryPath) || !Files.exists(packageMetadataRepositoryPath) || RepositoryFiles.hasRefreshContent(packageMetadataRepositoryPath)) {
                RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository, packageName);
                if (!Files.exists(repositoryPath)) {
                    return null;
                }
                npmPackageMetadataIndexer.indexAsSystem(repositoryPath, NpmIndexTypeEnum.REINDEX);
                packageMetadataRepositoryPath = repositoryPathResolver.resolve(repository, packageFeedFilePath);
                if (Objects.isNull(packageMetadataRepositoryPath) || !Files.exists(packageMetadataRepositoryPath)) {
                    return null;
                }
            }
            return npmComponent.readPackageFeed(packageMetadataRepositoryPath);
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
        return null;
    }

    @Override
    public PackageFeed getLocalPackageFeed(Repository repository, String packageName, String targetUrl) {
        return packageFeed(repository, packageName, targetUrl);
    }

    @Override
    public String binary(Repository repository, String packageName, String targetUrl) {
        return null;
    }

    @Override
    public String getLocalBinary(Repository repository, String packageName, String targetUrl) {
        return null;
    }

}
