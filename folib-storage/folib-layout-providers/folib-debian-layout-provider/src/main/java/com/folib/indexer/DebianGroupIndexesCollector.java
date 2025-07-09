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

import cn.hutool.extra.spring.SpringUtil;
import com.google.common.base.Joiner;
import com.google.common.collect.Sets;
import com.folib.configuration.ConfigurationUtils;
import com.folib.domain.DebianGroupMetadataWorkItem;
import com.folib.domain.DebianPackageFileMetadata;
import com.folib.enums.DebianArchiveFormat;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.services.ArtifactManagementService;
import com.folib.services.ArtifactResolutionService;
import com.folib.services.RepositoryManagementService;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryTypeEnum;
import com.folib.util.DebianUtils;
import com.folib.util.steam.MetadataStreamGz;
import com.folib.util.steam.StringInputStream;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.compress.compressors.bzip2.BZip2CompressorInputStream;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringUtils;
import org.tukaani.xz.LZMAInputStream;
import org.tukaani.xz.XZInputStream;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.zip.GZIPInputStream;

/**
 * @author veadan
 * @since 2025-02-27 14:10
 */
@Slf4j
@Data
public class DebianGroupIndexesCollector {

    private static final String PACKAGE_DELIMITER = "\\n\\n";
    private static final int PACKAGE_NAME_PREFIX_LENGTH = "Package: ".length();
    private Map<String, List<DebianPackageFileMetadata>> packagesFileChecksums=new HashMap<>();
    private Map<String, String> releaseFileChecksums=new HashMap<>();
    private Set<String> archsToRecalculate=new HashSet<>();
    private Set<String> compsToRecalculate=new HashSet<>();
    private List<String> compFoldersToDelete = new ArrayList<>();
    private Set<RepositoryPath> checksumPassedPackagesFiles;
    private Set<String> prioritisedPackages = new HashSet<>();
    //    private DebianAdapter repo;
    private String distribution;
    private String component;
    private Repository groupRepo;
    private boolean isPriorityResolutionEnabled;
    //    private InternalRepositoryService repoService;
    DebianGroupHelper debianGroupHelper;
    private RepositoryPathResolver repositoryPathResolver;

    private ArtifactResolutionService artifactResolutionService;

    private ArtifactManagementService artifactManagementService;

    private String tempPath;


    public DebianGroupIndexesCollector(Repository repo,String distribution, String component) {
        this.groupRepo = repo;
        this.distribution = distribution;
        this.component = component;
        this.repositoryPathResolver = SpringUtil.getBean(RepositoryPathResolver.class);
        this.debianGroupHelper =SpringUtil.getBean(DebianGroupHelper.class);
        this.artifactResolutionService = SpringUtil.getBean(ArtifactResolutionService.class);
        this.artifactManagementService = SpringUtil.getBean(ArtifactManagementService.class);

    }

    private void extractRemotePackagesFile(Map<String, Map<String, List<CharSequence>>> componentsToArchsToPackagesFileContent, Repository remoteRepo, String comp, Set<String> remoteArchsToCalculate) {
        //获取这个package的package包
        List<DebianArchiveFormat> packageArchiveFormats = List.of(DebianArchiveFormat.GZIP, DebianArchiveFormat.BZ2);
        remoteArchsToCalculate.forEach(arch -> {
            log.info("Starting to index architecture:{}", arch);
            String packagesFileTextPlain = null;
            RepositoryPath packagesRepoPath = null;
            String extension = null;
            for (DebianArchiveFormat debianArchiveFormat : packageArchiveFormats) {
                String packagesFilePath = Joiner.on("/").join("dists", distribution, comp, arch, "Packages." + debianArchiveFormat.ext);
                RepositoryPath packagesFileRepoPath = repositoryPathResolver.resolve(remoteRepo, packagesFilePath);
                try {
                    artifactResolutionService.resolvePath(packagesFileRepoPath);
                    if (Files.exists(packagesFileRepoPath)) {
                        packagesRepoPath=packagesFileRepoPath;
                        extension = debianArchiveFormat.ext;
                        break;
                    }
                } catch (Exception e) {
                    log.info("Failed to resolve package file {}", packagesRepoPath);
                }

            }
            if (extension != null) {
                packagesFileTextPlain = this.downloadRemotePackages(packagesRepoPath, extension);
            } else {
                String packagesFilePath = Joiner.on("/").join("dists", distribution, comp, arch, "Packages");
                RepositoryPath packagesFileRepoPath = repositoryPathResolver.resolve(remoteRepo, packagesFilePath);
                try{
                    artifactResolutionService.resolvePath(packagesFileRepoPath);
                    if (Files.exists(packagesFileRepoPath)) {
                        packagesFileTextPlain =  packagesFileTextPlain = this.downloadRemotePackagesAndSaveAsGz(packagesRepoPath, packagesRepoPath);
                    }
                }catch (Exception e){
                    log.info("Failed to resolve package file {}", packagesRepoPath);
                }
            }
            if (packagesFileTextPlain != null) {
                this.addPackagesPlainTextToCompMap(componentsToArchsToPackagesFileContent, comp, arch, packagesFileTextPlain);
                this.extractPackagesGZFileSha1(comp, arch, remoteRepo, packagesRepoPath);
            } else {
                log.debug("Couldn't decode {} to text plain from remote repo:{}, Skipping this architecture", packagesRepoPath, remoteRepo.getId());
            }

            log.info("Finished to index architecture:{}", arch);
        });
    }

    private void extractPackagesGZFileSha1(String comp, String arch, Repository remoteRepo, RepositoryPath packagesGZRepoPath) {
//        RepositoryPath sha1Path = getSha1Path(packagesGZRepoPath);
//        if(Files.exists(sha1Path)) {
//            try {
//                String sha1 = IOUtils.toString(Files.newInputStream(sha1Path), StandardCharsets.UTF_8);
//                this.packagesFileChecksums.get(remoteRepo.getId()).add(new DebianPackageFileMetadata(comp, sha1, arch));
//            } catch (IOException e) {
//                throw new RuntimeException(e);
//            }
//
//        }
    }




    private String parsePackageName(String packageText) {
        String packageName;
        try {
            packageName = packageText.substring(PACKAGE_NAME_PREFIX_LENGTH, packageText.indexOf("\n", PACKAGE_NAME_PREFIX_LENGTH));
        } catch (IndexOutOfBoundsException var4) {
            log.info("Unable to parse package: " + packageText);
            packageName = null;
        }

        return packageName;
    }

    private String downloadRemotePackages(RepositoryPath packagesRepoPath, String extension) {
        try {
            artifactResolutionService.resolvePath(packagesRepoPath);
            if (Files.exists(packagesRepoPath)) {
                try (InputStream in = Files.newInputStream(packagesRepoPath)) {
                    InputStream inputStream;
                    switch (extension) {
                        case "gz":
                            inputStream = new GZIPInputStream(in);
                            break;
                        case "xz":
                            inputStream=new XZInputStream(in);
                            break;
                        case "bz2":
                            inputStream=new BZip2CompressorInputStream(in);
                            break;
                        case "lzma":
                            inputStream=new LZMAInputStream(in);
                            break;
                        default:
                            inputStream=null;
                    }
                    if(inputStream!=null) {
                        return IOUtils.toString(inputStream, StandardCharsets.UTF_8);
                    }else {
                        return null;
                    }
                }
            }
        } catch (Exception e) {
            log.warn("Unable to retrieve Packages.{} from repository {}, reason:{}", extension, packagesRepoPath.getRepositoryId(), e.getMessage());
            log.debug("Unable to retrieve Packages.{} from repository {}", extension, packagesRepoPath.getRepositoryId(), e);
        }
        return null;
    }


    private void extractAllAvailableComponentsFromRemoteReleaseFile(Repository remoteRepo, String distribution,Set<String> remoteCompsToCalculate,Set<String> remoteArchsToCalculate) {
        String releasePath = Joiner.on("/").join("dists", distribution, "Release");
        RepositoryPath releaseFileRepoPath = repositoryPathResolver.resolve(remoteRepo, releasePath);

        try {
            artifactResolutionService.resolvePath(releaseFileRepoPath);
        } catch (Exception e) {
            log.info("Failed to resolve package file {}", releaseFileRepoPath);
        }
        if (Files.exists(releaseFileRepoPath)) {
            try {
                String packagesFileTextPlain = new String(Files.readAllBytes(releaseFileRepoPath), StandardCharsets.UTF_8);
                int archsStartIndex = packagesFileTextPlain.indexOf("Architectures: ");
                int archsEndIndex = packagesFileTextPlain.indexOf(System.lineSeparator(), archsStartIndex);
                String allArchitectures = packagesFileTextPlain.substring(archsStartIndex + "Architectures: ".length(), archsEndIndex);
                Set<String> archs=Sets.newHashSet(allArchitectures.split(" ")).stream().map(arch->"binary-" + arch).collect(Collectors.toSet());
                remoteCompsToCalculate.retainAll(archs);
                int componentsStartIndex = packagesFileTextPlain.indexOf("Components: ");
                int componentsEndIndex = packagesFileTextPlain.indexOf(System.lineSeparator(), componentsStartIndex);
                String allComponents = packagesFileTextPlain.substring(componentsStartIndex + "Components: ".length(), componentsEndIndex);
                remoteCompsToCalculate.addAll(Sets.newHashSet(allComponents.split(" ")));

            } catch (IOException e) {
                log.error("couldn't extract available components from remote Release: {}", releaseFileRepoPath);
            }
        }
        log.error("couldn't find Release file from remote repo: {}", releaseFileRepoPath);
    }

    Map<String, Map<String, List<CharSequence>>> collectPackagesFilesFromAggregatedRepos() {
        Map<String, Map<String, List<CharSequence>>> componentsToArchsToPackagesFileContent = new HashMap();
        // 获取本地仓库
        Set<Repository> localRepos = new HashSet<>();
        Set<Repository> remoteRepos = new HashSet<>();
        this.getResolvedLocalAndRemoteRepos(groupRepo, localRepos,remoteRepos, new HashSet<>());
        this.addOrRemoveCompsAndArchsFromLocalRepo(localRepos);
        // 获取优先级
//        Map<RepoPriority, Set<RealRepo>> reposToResolve = PriorityUtils.resolveRepos(this.virtualRepo, true, RepoType.Debian, LinkedHashSet::new);
//        if (!((Set)reposToResolve.get(RepoPriority.PRIORITISED_LOCAL)).isEmpty() || !((Set)reposToResolve.get(RepoPriority.PRIORITISED_REMOTE)).isEmpty()) {
//            this.isPriorityResolutionEnabled = true;
//            this.appendMissingPackagesFilesFromLocalRepos(componentsToArchsToPackagesFileContent, (Set)reposToResolve.get(RepoPriority.PRIORITISED_LOCAL));
//            this.collectPackagesFilesFromRemoteRepos(componentsToArchsToPackagesFileContent, (Set)reposToResolve.get(RepoPriority.PRIORITISED_REMOTE));
//        }
        this.appendMissingPackagesFilesFromLocalRepos(componentsToArchsToPackagesFileContent, localRepos);
        this.collectPackagesFilesFromRemoteRepos(componentsToArchsToPackagesFileContent, remoteRepos);
        return componentsToArchsToPackagesFileContent;
    }


    private void collectPackagesFilesFromRemoteRepos(Map<String, Map<String, List<CharSequence>>> componentsToArchsToPackagesFileContent, Set<Repository> remoteRepos) {
        for (Repository remoteRepo : remoteRepos) {
            Set<String> remoteCompsToCalculate = new HashSet<>();
            Set<String> remoteArchsToCalculate = calcRemoteArcsToCalculate();
            this.extractAllAvailableComponentsFromRemoteReleaseFile(remoteRepo, this.distribution,remoteCompsToCalculate,remoteArchsToCalculate);
            if (remoteArchsToCalculate.isEmpty()) {
                log.error("Could not collect Release/Packages files from remote:{}, skipping repo", remoteRepo.getId());
                continue;
            }
            log.debug("Starting to reindex the next components:{}, architectures:{}", remoteCompsToCalculate, remoteArchsToCalculate);
            remoteCompsToCalculate.forEach((comp) -> this.extractRemotePackagesFile(componentsToArchsToPackagesFileContent, remoteRepo, comp, remoteArchsToCalculate));
        }
    }

    // 获取默认的架构
    private Set<String> calcRemoteArcsToCalculate() {
        Set<String> remoteArcsToCalculate = new HashSet<>();
        DebianUtils.getDebianDefaultArchitectures(this.groupRepo).forEach((arc) -> remoteArcsToCalculate.add("binary-" + arc));
        return remoteArcsToCalculate;
    }

    private void appendMissingPackagesFilesFromLocalRepos(Map<String, Map<String, List<CharSequence>>> componentsToArchsToPackagesFileContent, Set<Repository> localRepos) {
        for (Repository localRepo : localRepos) {
            this.appendMissingPackagesFilesFromLocalRepo(componentsToArchsToPackagesFileContent, localRepo);
        }
    }

    void appendMissingPackagesFilesFromLocalRepo(Map<String, Map<String, List<CharSequence>>> componentsToArchsToPackagesFileContent, Repository localRepo) {
        for (String comp : this.compsToRecalculate) {
            String pathToComponentFolder = Joiner.on("/").join("dists", this.distribution, comp);
            RepositoryPath pathToComponentRepoFolder = this.repositoryPathResolver.resolve(localRepo, pathToComponentFolder);
            List<Path> packageFiles = new ArrayList<>();
            try (Stream<Path> paths = Files.walk(pathToComponentRepoFolder, 2)) {
                packageFiles = paths.filter(Files::isRegularFile).filter(path -> path.getFileName().toString().equals("Packages")).collect(Collectors.toList());
            } catch (Exception e) {
                log.error("couldn't extract package files from repo {}", pathToComponentRepoFolder, e);
            }
            // 读package内容
            Map<String, String> archToPackagesFileContent = this.readPackagesFileAndExtractSha1(packageFiles);
            this.reducePackagesFilesByComponents(componentsToArchsToPackagesFileContent, comp, archToPackagesFileContent, false);
        }
    }


    private Map<String, String> readPackagesFileAndExtractSha1(List<Path> packagesArtifacts) {
        Map<String, String> archToPackageFileContent = new HashMap<>();
        for (Path packagesArtifact : packagesArtifacts) {
            if (packagesArtifact instanceof RepositoryPath) {
                RepositoryPath repoPath = (RepositoryPath) packagesArtifact;
                try (InputStream packageFileInputStream = Files.newInputStream(repoPath)) {
                    String arch = DebianUtils.getArchFromPackagesFileRepoPath(repoPath);
                    String comp = DebianUtils.getComponentFromPackagesRepoPath(repoPath);
                    archToPackageFileContent.put(arch, IOUtils.toString(packageFileInputStream, StandardCharsets.UTF_8));
                    this.extractPackagesFileSha1(repoPath, arch, comp);
                } catch (Exception e) {
                    log.error("couldn't extract package file from repo {}", packagesArtifact, e);
                }
            }
        }
        return archToPackageFileContent;
    }

    // 提取sha1
    private void extractPackagesFileSha1(RepositoryPath packageFileRepoPath, String arc, String comp) {
        RepositoryPath sha1Path = getSha1Path(packageFileRepoPath);
        if (Files.isExecutable(sha1Path)) {
            try {
                String sha1 = new String(Files.readAllBytes(sha1Path), StandardCharsets.UTF_8);
                log.info("Found checksum {} for Packages file at path {}", sha1, packageFileRepoPath);
                this.packagesFileChecksums.putIfAbsent(packageFileRepoPath.getRepositoryId(), new ArrayList());
                this.packagesFileChecksums.get(packageFileRepoPath.getRepositoryId()).add(new DebianPackageFileMetadata(comp, sha1, arc));
            } catch (Exception e) {
                log.error("couldn't extract sha1 file from repo {}", packageFileRepoPath, e);
            }

        }
        String releaseFileRelPath = Joiner.on("/").join("dists", this.distribution, "Release");
        RepositoryPath releaseArtifact = packageFileRepoPath.getRoot().resolve(releaseFileRelPath);
        RepositoryPath releaseSha1 = getSha1Path(releaseArtifact);
        if (Files.exists(releaseSha1)) {
            try {
                String sha1 = new String(Files.readAllBytes(releaseSha1), StandardCharsets.UTF_8);
                this.releaseFileChecksums.put(packageFileRepoPath.getRepositoryId(), sha1);
            } catch (Exception e) {
                log.info("Error retrieving SHA1 of Release file from:{}, probably the local repo is modifying this file, firing new reindex event.", releaseFileRelPath);
            }
        } else {
            log.info("Error retrieving SHA1 of Release file from:{}, probably the local repo is modifying this file, firing new reindex event.", releaseFileRelPath);
        }
    }

    private void reducePackagesFilesByComponents(Map<String, Map<String, List<CharSequence>>> componentsToArchsToPackagesFileContent, String component, Map<String, String> archToPackagesFileContent, boolean isPriority) {
        archToPackagesFileContent.forEach((arch, packageFileContent) -> this.addPackagesPlainTextToCompMap(componentsToArchsToPackagesFileContent, component, arch, packageFileContent));
    }

    private void addPackagesPlainTextToCompMap(Map<String, Map<String, List<CharSequence>>> componentsToArchsToPackagesFileContent, String comp, String arch, String packagesFileTextPlain) {
        componentsToArchsToPackagesFileContent.putIfAbsent(comp, new HashMap<>());
        Map<String, List<CharSequence>> archToPackageFileContent = componentsToArchsToPackagesFileContent.get(comp);
        archToPackageFileContent.putIfAbsent(arch, new ArrayList<>());
        archToPackageFileContent.get(arch).add(packagesFileTextPlain);
    }


    // 获取所有的本地仓库 递归获取
    void getResolvedLocalAndRemoteRepos(Repository groupRepo, Set<Repository> resolvedLocalRepos, Set<Repository> resolvedRemoteRepos, Set<String> resolvedGroupRepos) {
        if (!RepositoryTypeEnum.GROUP.getType().equals(groupRepo.getType())) {
            return;
        }
        String storageIdAndRepositoryId = groupRepo.getStorageIdAndRepositoryId();
        if (!resolvedGroupRepos.contains(storageIdAndRepositoryId)) {
            RepositoryManagementService repositoryManagementService = SpringUtil.getBean(RepositoryManagementService.class);
            Set<String> groupRepositories = groupRepo.getGroupRepositories();
            for (String storageAndRepo : groupRepositories) {
                String storageId = ConfigurationUtils.getStorageId(storageAndRepo, storageAndRepo);
                String repositoryId = ConfigurationUtils.getRepositoryId(storageAndRepo);
                Repository repository = repositoryManagementService.getStorage(storageId).getRepository(repositoryId);
                if (RepositoryTypeEnum.HOSTED.getType().equals(repository.getType())) {
                    resolvedLocalRepos.add(repository);
                } else if (RepositoryTypeEnum.GROUP.getType().equals(repository.getType())) {
                    getResolvedLocalAndRemoteRepos(repository, resolvedLocalRepos,resolvedRemoteRepos, resolvedGroupRepos);
                }else if(RepositoryTypeEnum.PROXY.getType().equals(repository.getType())){
                    resolvedRemoteRepos.add(repository);
                }
            }
        }
    }


    private void addOrRemoveCompsAndArchsFromLocalRepo(Set<Repository> localRepo) {
        Set<String> localComponents = new HashSet<>();
        for (Repository localOrCacheRepo : localRepo) {
            localComponents.addAll(this.extractAllAvailableComponentsFromLocalRepo(localOrCacheRepo, this.distribution));
        }
        this.compsToRecalculate.addAll(localComponents);
        // 获取本地不存的组件
        if(!this.compsToRecalculate.isEmpty()){
            List<String> compsNotInLocal = this.compsToRecalculate.stream().filter((virtualComponent) -> !localComponents.contains(virtualComponent)).collect(Collectors.toList());
            this.compsToRecalculate.removeAll(compsNotInLocal);
            this.compFoldersToDelete.addAll(compsNotInLocal);
        }
    }


    // 获取发行版下所有的组件
    Set<String> extractAllAvailableComponentsFromLocalRepo(Repository repository, String distribution) {
        String distributionPath = Joiner.on("/").join("dists", distribution);
        RepositoryPath distributionRepoPath = this.repositoryPathResolver.resolve(repository, distributionPath);
        // 获取发行版下所有的文件夹 名字就是组件名
        if(Files.exists(distributionRepoPath)){
            try (Stream<Path> repositoryPathStream = Files.walk(distributionRepoPath, 1)) {
                return repositoryPathStream.filter(Files::isDirectory) // 只保留目录
                        .filter(path -> !path.equals(distributionRepoPath)) // 排除自身
                        .map(p -> p.getFileName().toString()).collect(Collectors.toSet());
            } catch (IOException exception) {
                log.info("Unable to extract available components from local repo {}", distributionRepoPath);
            }
        }
        return new HashSet<>();
    }

    RepositoryPath getSha1Path(RepositoryPath repo) {
        if (Files.isRegularFile(repo)) {
            String filename = repo.getFileName().toString();
            String sha1Name = filename + ".sha1";
        }
        return repo;
    }

    private String downloadRemotePackagesAndSaveAsGz( RepositoryPath packagesRepoPath, RepositoryPath packagesGZRepoPath) {
        String packagesPlainText=null;
        if(Files.exists(packagesRepoPath)) {
            try {
                packagesPlainText = IOUtils.toString(Files.newInputStream(packagesRepoPath), Charset.defaultCharset());
                artifactManagementService.store(packagesGZRepoPath, new MetadataStreamGz(new StringInputStream(packagesPlainText)));
            } catch (Exception ex) {
                log.error("Couldn't save Packages file to remote cache due to:{}", ex.getMessage());
            }
        }
        return packagesPlainText;
    }


    void collectPackagesFilesToRecalculate(DebianGroupMetadataWorkItem workItem) {
        Set<RepositoryPath> existingPackagesInPath = this.debianGroupHelper.extractExistingPackagesInPath(this.groupRepo, this.distribution);
        if (!existingPackagesInPath.isEmpty()) {
            for(RepositoryPath packagesFileRepoPath : existingPackagesInPath) {
                try {
                    String arch = DebianUtils.getArchFromPackagesFileRepoPath(packagesFileRepoPath);
                    String comp = DebianUtils.getComponentFromPackagesRepoPath(packagesFileRepoPath);
                    this.archsToRecalculate.add(arch);
                    this.compsToRecalculate.add(comp);
                } catch (InvalidPathException exception) {
                    log.warn("Couldn't extract architecture/component from path, skipping {} from calculations", packagesFileRepoPath.getPath());
                }
            }

            if (!this.compsToRecalculate.contains(workItem.getComponent()) && StringUtils.isNotEmpty(workItem.getComponent())) {
                this.compsToRecalculate.add(workItem.getComponent());
            }

        }
    }

    RepositoryPath buildPackagesFileRepoPath(String component, String architecture, String extension) {
        String path=Joiner.on("/").join("dists", this.distribution, component, architecture, "Packages" + (extension == null ? "" : extension));
        return this.repositoryPathResolver.resolve(this.groupRepo,path);
    }

    void copyMissingPackagesFromVirtualCacheToVirtualCacheTempFolder() {
//        for(RepositoryPath repoPathToValidatedPackagesFile : this.checksumPassedPackagesFiles) {
//            try {
//                String arch = DebianUtils.getArchFromPackagesFileRepoPath(repoPathToValidatedPackagesFile);
//                String comp = DebianUtils.getComponentFromPackagesRepoPath(repoPathToValidatedPackagesFile);
//                if (!this.archsToRecalculate.contains(arch) || !this.compsToRecalculate.contains(comp)) {
//                    RepositoryPath virtualCacheArchPath = this.buildPackagesFileRepoPath(comp, arch, null).getParent();
//                    List<ItemInfo> allFilesUnderArchFolder = this.repoService.getChildrenDeeply(repoPathToValidatedPackagesFile.getParent());
//                    String relativePathToArchFolder = DebianUtils.getRelativePathToArch(repoPathToValidatedPackagesFile);
//                    allFilesUnderArchFolder.stream().filter((itemInfo) -> !itemInfo.isFolder() && !itemInfo.getRelPath().contains("by-hash")).forEach((itemInfo) -> {
//                        if (virtualCacheArchPath != null) {
//                            String to = virtualCacheArchPath.getPath() + itemInfo.getRepoPath().getPath().replace(relativePathToArchFolder, "");
//                            if (this.repo.copy(itemInfo.getRepoPath().getPath(), to)) {
//                                ((DebianService)ContextHelper.get().beanForType(DebianService.class)).copyPropertiesToVirtualCacheFile(this.virtualRepo, itemInfo.getRepoPath(), RepoPathFactory.create(this.virtualRepo.getKey(), to));
//                            } else {
//                                log.error("Failed to copy file from:{} to:{}", itemInfo.getRepoPath(), virtualCacheArchPath);
//                            }
//                        }
//                    });
//                }
//            } catch (InvalidPathException var8) {
//                log.error("Couldn't extract component/distribution from repo path:{}, Skipping coping missing Packages, This may cause malformed Release file in virtual cache", repoPathToValidatedPackagesFile);
//            }
//        }

    }


}
