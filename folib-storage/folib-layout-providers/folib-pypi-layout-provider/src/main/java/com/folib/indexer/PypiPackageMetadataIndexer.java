/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.indexer;

import com.folib.artifact.coordinates.PypiCoordinates;
import com.folib.components.DistributedLockComponent;
import com.folib.components.PypiBrowsePackageHtmlResponseBuilder;
import com.folib.configuration.ConfigurationManager;
import com.folib.constant.GlobalConstants;
import com.folib.constants.PypiConstants;
import com.folib.data.criteria.Paginator;
import com.folib.enums.PypiIndexTypeEnum;
import com.folib.model.PypiIndexEntry;
import com.folib.model.PypiSimpleIndex;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.providers.repository.RepositoryProvider;
import com.folib.providers.repository.RepositoryProviderRegistry;
import com.folib.providers.repository.RepositorySearchRequest;
import com.folib.services.PypiService;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryTypeEnum;
import com.folib.util.PypiUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import javax.validation.constraints.NotNull;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Objects;
import java.util.SortedSet;

/**
 * @author veadan
 **/
@Slf4j
@Component
public class PypiPackageMetadataIndexer {

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private RepositoryProviderRegistry repositoryProviderRegistry;

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    @Lazy
    private PypiService pypiService;

    @Inject
    private DistributedLockComponent distributedLockComponent;

    @Inject
    @Lazy
    private PypiBrowsePackageHtmlResponseBuilder pypiBrowsePackageHtmlResponseBuilder;

    public void indexAsSystem(RepositoryPath repositoryPath, PypiIndexTypeEnum pypiIndexTypeEnum) throws Exception {
        if (Objects.isNull(repositoryPath) || Files.isHidden(repositoryPath) || RepositoryFiles.isMetadata(repositoryPath)) {
            return;
        }
        if (!RepositoryTypeEnum.HOSTED.getType().equals(repositoryPath.getRepository().getType())) {
            return;
        }
        String storageId = repositoryPath.getStorageId(), repositoryId = repositoryPath.getRepositoryId();
        PypiCoordinates pypiArtifactCoordinates = null;
        String artifactPath = RepositoryFiles.relativizePath(repositoryPath);
        if (artifactPath.startsWith(PypiConstants.PACKAGE_HTML_EXTENSION)) {
            return;
        }
        if (PypiCoordinates.EXTENSION_LIST.stream().noneMatch(artifactPath::endsWith)) {
            pypiArtifactCoordinates = PypiCoordinates.resolveName(artifactPath);
        } else {
            pypiArtifactCoordinates = PypiCoordinates.parse(artifactPath);
        }
        final String packageName = PypiUtils.escapeSpecialCharacters(pypiArtifactCoordinates.getId());
        String key = String.format("PypiIndex_%s_%s_%s", storageId, repositoryId, packageName), htmlData = "";
        PypiSimpleIndex pypiSimpleIndex = null;
        if (distributedLockComponent.lock(key, GlobalConstants.WAIT_LOCK_TIME)) {
            try {
                if (shouldReindexPackage(pypiIndexTypeEnum)) {
                    log.trace("Found Reindex for package '{}' in storage '{}' repository '{}'", packageName, storageId, repositoryId);
                    htmlData = reindexPackage(repositoryPath, storageId, repositoryId, packageName);
                } else {
                    pypiSimpleIndex = extractPackageMetadata(repositoryPath, packageName);
                    log.debug("Started indexing Pypi package '{}' in storage '{}' repository '{}'", packageName, storageId, repositoryId);
                    indexType(repositoryPath, pypiArtifactCoordinates, storageId, repositoryId, packageName, pypiIndexTypeEnum, pypiSimpleIndex);
                    log.debug("Finished indexing Pypi package '{}' in storage '{}' repository '{}'", packageName, storageId, repositoryId);
                    htmlData = pypiSimpleIndex.toString();
                }
                finalizePackageIndexing(repositoryPath, pypiArtifactCoordinates, storageId, repositoryId, packageName, pypiIndexTypeEnum, htmlData);
            } finally {
                distributedLockComponent.unLock(key);
            }
        }
    }

    private String reindexPackage(RepositoryPath repositoryPath, String storageId, String repositoryId, String packageName) throws Exception {
        log.info("Started reindexing for package '{}' in storage '{}' repository '{}'", packageName, storageId, repositoryId);
        String htmlData = null, searchPackageName = packageName + "-";
        RepositoryProvider repositoryProvider = repositoryProviderRegistry.getProvider(repositoryPath.getRepository().getType());
        RepositorySearchRequest predicate = new RepositorySearchRequest(searchPackageName, true, PypiCoordinates.EXTENSION_LIST);
        Paginator paginator = new Paginator();
        List<Path> searchResult = repositoryProvider.search(storageId, repositoryId,
                predicate, paginator);
        if (CollectionUtils.isNotEmpty(searchResult)) {
            try {
                htmlData = pypiBrowsePackageHtmlResponseBuilder.getHtmlResponse(searchResult);
            } catch (Exception ex) {
                log.error(ExceptionUtils.getStackTrace(ex));
            }
        } else {
            String packageMetadataFilePath = PypiUtils.getPackageIndexPathLocalRepo(packageName);
            RepositoryPath packageJsonRepositoryPath = repositoryPathResolver.resolve(repositoryPath.getRepository(), packageMetadataFilePath);
            Files.deleteIfExists(packageJsonRepositoryPath);
        }
        return htmlData;
    }

    private boolean shouldReindexPackage(PypiIndexTypeEnum pypiIndexTypeEnum) {
        return PypiIndexTypeEnum.REINDEX.getType().equals(pypiIndexTypeEnum.getType());
    }

    private void finalizePackageIndexing(RepositoryPath repositoryPath, PypiCoordinates pypiArtifactCoordinates, String storageId, String repositoryId, String packageName, PypiIndexTypeEnum pypiIndexTypeEnum, String htmlData) throws Exception {
        writeOrDeletePackageMetadataFile(repositoryPath, pypiArtifactCoordinates, storageId, repositoryId, packageName, pypiIndexTypeEnum, htmlData);
    }

    private void indexType(RepositoryPath repositoryPath, PypiCoordinates pypiArtifactCoordinates, String storageId, String repositoryId, String packageName, PypiIndexTypeEnum pypiIndexTypeEnum, PypiSimpleIndex pypiSimpleIndex) {
        try {
            switch (pypiIndexTypeEnum.getType()) {
                case "add":
                    handleAddPypiPackage(pypiSimpleIndex, repositoryPath, pypiArtifactCoordinates, packageName);
                    break;
                case "delete":
                    handleDeletePypiPackage(pypiSimpleIndex, repositoryPath, pypiArtifactCoordinates, packageName);
                    break;
                default:
                    ;
            }
        } catch (Exception e) {
            log.warn("Error occurred during index processing error [{}]", ExceptionUtils.getStackTrace(e));
        }
    }

    private void writeOrDeletePackageMetadataFile(RepositoryPath repositoryPath, PypiCoordinates pypiArtifactCoordinates, String storageId, String repositoryId, String packageName, PypiIndexTypeEnum pypiIndexTypeEnum, String htmlData) throws Exception {
        String packageMetadataFilePath = PypiUtils.getPackageIndexPathLocalRepo(packageName);
        RepositoryPath packageHtmlRepositoryPath = repositoryPathResolver.resolve(repositoryPath.getRepository(), packageMetadataFilePath);
        if (StringUtils.isNotBlank(htmlData)) {
            log.trace("Writing metadata to: '{}'", packageHtmlRepositoryPath.toString());
            try {
                Files.writeString(packageHtmlRepositoryPath, htmlData);
            } catch (IOException e) {
                log.error("Could not write to '{}' because of the following [{}]", packageName, ExceptionUtils.getStackTrace(e));
            }
        } else {
            log.trace("Metadata for package contains no versions, deleting file in '{}'", packageHtmlRepositoryPath);
            Files.deleteIfExists(packageHtmlRepositoryPath);
        }
    }

    private void handleDeletePypiPackage(PypiSimpleIndex pypiSimpleIndex, RepositoryPath repositoryPath, PypiCoordinates pypiArtifactCoordinates, String packageName) throws Exception {
        log.debug("Handling Delete for package: '{}', version: '{}'", packageName, pypiArtifactCoordinates.getVersion());
        removeVersionFromPackageMetadata(repositoryPath, pypiArtifactCoordinates, pypiSimpleIndex);
    }

    private void handleAddPypiPackage(PypiSimpleIndex pypiSimpleIndex, RepositoryPath repositoryPath, PypiCoordinates pypiArtifactCoordinates, String packageName) throws Exception {
        log.debug("Handling Add for package: '{}', version: '{}'", packageName, pypiArtifactCoordinates.getVersion());
        addPypiPackage(repositoryPath, pypiArtifactCoordinates, pypiSimpleIndex);
    }

    private void addPypiPackage(RepositoryPath repositoryPath, PypiCoordinates pypiArtifactCoordinates, PypiSimpleIndex pypiSimpleIndex) throws Exception {
        String repositoryBaseUrl = getRepositoryBaseUrl(repositoryPath.getRepository());
        String link = repositoryBaseUrl + PypiConstants.PYPI_PACKAGES + GlobalConstants.SEPARATOR + pypiArtifactCoordinates.getFileName();
        PypiIndexEntry pypiIndexEntry = PypiIndexEntry.builder().link(link).name(pypiArtifactCoordinates.getFileName()).build();
        pypiSimpleIndex.getEntries().add(pypiIndexEntry);
    }

    private void removeVersionFromPackageMetadata(RepositoryPath repositoryPath, PypiCoordinates pypiArtifactCoordinates, PypiSimpleIndex pypiSimpleIndex) {
        log.debug("Remove version {} from package {}", pypiArtifactCoordinates.getVersion(), pypiArtifactCoordinates.getFileName());
        SortedSet<PypiIndexEntry> pypiIndexEntries = pypiSimpleIndex.getEntries();
        String repositoryBaseUrl = getRepositoryBaseUrl(repositoryPath.getRepository());
        String link = repositoryBaseUrl + PypiConstants.PYPI_PACKAGES + GlobalConstants.SEPARATOR + pypiArtifactCoordinates.getFileName();
        PypiIndexEntry pypiIndexEntry = PypiIndexEntry.builder().link(link).name(pypiArtifactCoordinates.getFileName()).build();
        pypiIndexEntries.remove(pypiIndexEntry);
    }

    @NotNull
    private PypiSimpleIndex extractPackageMetadata(RepositoryPath repositoryPath, String packageName) throws Exception {
        String packageMetadataFilePath = PypiUtils.getPackageIndexPathLocalRepo(packageName);
        RepositoryPath packageHtmlRepositoryPath = repositoryPathResolver.resolve(repositoryPath.getRepository(), packageMetadataFilePath);
        if (Objects.nonNull(packageHtmlRepositoryPath) && Files.exists(packageHtmlRepositoryPath)) {
            try (InputStream inputStream = Files.newInputStream(packageHtmlRepositoryPath)) {
                return PypiIndexReader.read(inputStream);
            }
        }
        PypiSimpleIndex pypiSimpleIndex = new PypiSimpleIndex();
        return pypiSimpleIndex;
    }

    protected String getRepositoryBaseUrl(Repository repository) {
        return String.format("%s/storages/%s/%s", StringUtils.chomp(configurationManager.getConfiguration().getBaseUrl(), "/"), repository.getStorage().getId(), repository.getId());
    }

}

