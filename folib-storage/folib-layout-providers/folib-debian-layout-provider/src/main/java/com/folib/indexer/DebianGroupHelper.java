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

import com.google.common.base.Joiner;
import com.folib.domain.DebianGroupMetadataWorkItem;
import com.folib.enums.DebianArchiveFormat;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.services.ArtifactManagementService;
import com.folib.storage.repository.Repository;
import com.folib.util.DebianUtils;
import com.folib.util.steam.StringInputStream;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.io.InputStream;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author veadan
 * @since 2025-02-27 14:09
 */

@Slf4j
@Component
public class DebianGroupHelper {

    private final ArtifactManagementService artifactManagementService;

    private final RepositoryPathResolver repositoryPathResolver;
    @Autowired
    public DebianGroupHelper(ArtifactManagementService artifactManagementService,RepositoryPathResolver repositoryPathResolver) {
        this.artifactManagementService = artifactManagementService;
        this.repositoryPathResolver = repositoryPathResolver;
    }

    Map<String, Map<String, String>> mergePackagesFilesByArch(Map<String, Map<String, List<CharSequence>>> componentsToArchsToPackagesFileContent) {
        Map<String, Map<String, String>> compToArchToMergedPackageFile = new HashMap<>();
        Set<String> components = componentsToArchsToPackagesFileContent.keySet();
        components.forEach((comp) -> {
            Map<String, List<CharSequence>> archsToPackagesFiles = componentsToArchsToPackagesFileContent.get(comp);
            archsToPackagesFiles.forEach((arch, packagesFilesList) -> {
                compToArchToMergedPackageFile.putIfAbsent(comp, new HashMap<>());
                Map<String, String> archToPackagesFileContent = compToArchToMergedPackageFile.get(comp);
                archToPackagesFileContent.put(arch, String.join("", packagesFilesList));
            });
        });
        return compToArchToMergedPackageFile;
    }

    void persistMergedPackageFiles(DebianGroupIndexesCollector debianGroupIndexesCollector, Map<String, Map<String, String>> compToArchToMergedPackageFile) {
        Set<String> components = compToArchToMergedPackageFile.keySet();
        components.forEach((comp) -> {
            Map<String, String> archToMergedPackageFile = compToArchToMergedPackageFile.get(comp);
            archToMergedPackageFile.forEach((arch, mergedPackagesFileContentPlainText) -> {
                RepositoryPath packagesRepoPath = debianGroupIndexesCollector.buildPackagesFileRepoPath(comp, arch, null);
                this.savePackagesFile(packagesRepoPath, new StringInputStream(mergedPackagesFileContentPlainText));
                this.savePackagesFileAccordingArchiveFormats(debianGroupIndexesCollector, mergedPackagesFileContentPlainText, comp, arch);
            });
        });
    }

    private void savePackagesFile(RepositoryPath packagesRepoPath, InputStream packagesInputStream) {
        try {
            log.debug("Starting to save merged Packages file in path {}/{}", packagesRepoPath.getRepositoryId(), packagesRepoPath.getPath());
            this.artifactManagementService.store(packagesRepoPath, packagesInputStream);
            log.debug("Saved merged Packages file in path {}/{}", packagesRepoPath.getRepositoryId(), packagesRepoPath.getPath());
        } catch (Exception e) {
            log.error("Unable to save merged Packages file in path {}/{}", packagesRepoPath.getRepositoryId(), packagesRepoPath.getPath());
            log.debug("", e);
        }

    }

    private void savePackagesFileAccordingArchiveFormats(DebianGroupIndexesCollector  debianGroupIndexesCollector, String packageFileContent, String comp, String arch) {
        List<DebianArchiveFormat> packagesArchiveFormats = DebianUtils.getPackagesArchiveFormats();
        packagesArchiveFormats.forEach((format) -> {
            RepositoryPath packagesRepoPath = debianGroupIndexesCollector.buildPackagesFileRepoPath(comp, arch, "." + format.ext);
            try (InputStream packagesInputStream = format.createPackagesIndexInputStream(packageFileContent)) {
                this.artifactManagementService.store(packagesRepoPath, packagesInputStream);
            } catch (IOException e) {
                log.error("Unable to save merged Packages file in path {}/{}", packagesRepoPath.getRepositoryId(), packagesRepoPath.getPath(), e);
            }

        });
    }

    Set<RepositoryPath> extractExistingPackagesInPath(Repository groupRepo, String distribution) {
//        RepositoryPath distributionRepoPath =repositoryPathResolver.resolve(groupRepo, Joiner.on("/").join("dists", distribution));
//        List<ItemInfo> allItemsInDistributionPath = this.repoService.getChildrenDeeply(distributionRepoPath);
//        Set<RepoPath> packagesFile = new HashSet();
//
//        for(ItemInfo itemInfo : allItemsInDistributionPath) {
//            if (!itemInfo.isFolder() && itemInfo.getName().equals("Packages") && !DebianUtils.getComponentFromPackagesRepoPath(itemInfo.getRepoPath()).contains("_temp-")) {
//                packagesFile.add(itemInfo.getRepoPath());
//            }
//        }
//
//        return packagesFile;
        return Collections.EMPTY_SET;
    }

    void validateParentVirtualRepository(DebianGroupMetadataWorkItem workItem, Repository virtualRepo) {
//        if (virtualRepo == null) {
//            throw new NullPointerException("virtualRepo is marked non-null but is null");
//        } else {
//            String distribution = workItem.getDistribution();
//            String component = workItem.getComponent();
//            String virtualRepoKey = virtualRepo.getKey();
//            List<VirtualRepoConfig> associatedVirtualRepos = this.getAssociatedVirtualRepos(workItem.getRepoKey(), 1);
//            associatedVirtualRepos.remove(virtualRepo.getRepoConfig());
//            if (!associatedVirtualRepos.isEmpty()) {
//                log.info("RepoKey:{} has parent virtual repositories:{}, Invoking recalculating indexes ", associatedVirtualRepos, virtualRepoKey);
//
//                for(VirtualRepoConfig associatedVirtualRepo : associatedVirtualRepos) {
//                    this.getDebianService().calculateDebianVirtualMetadataAsync(new DebianVirtualMetadataWorkItem(associatedVirtualRepo.getKey(), distribution, component, workItem.getPassphrase()));
//                }
//            }
//
//        }
    }

    void extractExistingArchsAndCompsInTempPath(Repository groupRepo, DebianGroupIndexesCollector debianVirtualIndexesCollector, Set<String> compsInTempPath, Set<String> archsInTempPath) {
        String distribution = debianVirtualIndexesCollector.getDistribution();
        RepositoryPath distributionRepoPath = repositoryPathResolver.resolve(groupRepo, Joiner.on("/").join("dists", distribution));

//        for(ItemInfo itemInfo : this.repoService.getChildrenDeeply(distributionRepoPath)) {
//            if (!itemInfo.isFolder() && itemInfo.getName().equals("Packages")) {
//                try {
//                    archsInTempPath.add(DebianUtils.getArchFromPackagesFileRepoPath(itemInfo.getRepoPath()));
//                    compsInTempPath.add(DebianUtils.getComponentFromPackagesRepoPath(itemInfo.getRepoPath()));
//                } catch (InvalidPathException var12) {
//                    log.error("Couldn't extract architecture/component from path, skipping {} from calculations", itemInfo.getRepoPath());
//                }
//            }
//        }

    }







}
