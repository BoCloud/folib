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
package com.folib.extractor;


import com.fasterxml.jackson.databind.ObjectMapper;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import javax.annotation.Nonnull;


import com.folib.model.CargoDependencyMetadata;
import com.folib.model.CargoMetadata;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.services.ArtifactManagementService;
import com.folib.services.ArtifactResolutionService;
import com.folib.services.ArtifactService;

import com.folib.utils.CargoConstants;
import com.folib.utils.CargoUtil;
import lombok.Generated;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class CargoMetadataIndexer {

    @Generated
    private static final Logger log = LoggerFactory.getLogger(CargoMetadataIndexer.class);

    private final ArtifactService artifactService;
    private final ArtifactManagementService artifactManagementService;
    protected final RepositoryPathResolver repositoryPathResolver;
    protected final ArtifactResolutionService artifactResolutionService;

    @Autowired
    public CargoMetadataIndexer(ArtifactService artifactService,
                                ArtifactManagementService artifactManagementService,
                                RepositoryPathResolver repositoryPathResolver,
                                ArtifactResolutionService artifactResolutionService) {
        this.artifactService = artifactService;
        this.artifactManagementService = artifactManagementService;
        this.repositoryPathResolver = repositoryPathResolver;
        this.artifactResolutionService = artifactResolutionService;
    }

    public void index(RepositoryPath repositoryPath, CargoIndex[] indexEvents) {
        indexAsSystem(repositoryPath, indexEvents);
    }

    private void indexAsSystem(RepositoryPath repositoryPath, CargoIndex[] indexEvents) {
        if (indexEvents == null || indexEvents.length == 0) {
            log.debug("Skipping Cargo metadata indexing, events list is null");
            return;
        }
        CargoMetadataExtractor cargoMetadataExtractor = new CargoMetadataExtractor();
        for (CargoIndex event : indexEvents)
            indexEventAsSystem(repositoryPath, cargoMetadataExtractor, (CargoIndex) event);
        //pushChangesIfNeeded(context);
    }

    public void indexAsSystem(RepositoryPath repositoryPath, CargoIndex index) {
        CargoMetadataExtractor cargoMetadataExtractor = new CargoMetadataExtractor();
        indexEventAsSystem(repositoryPath, cargoMetadataExtractor, index);
        //pushChangesIfNeeded(context);
    }

    //private void pushChangesIfNeeded(RepoWorkContext context) {
    //    if (!context.getRepoKey().endsWith("-cache") && !isInternalIndex(context))
    //        this.repoService.gitHandler().pushChanges(context.getRepoKey());
    //}

    /**
     * 将事件作为系统索引的一部分进行处理
     * 此方法负责使用提供的CargoMetadataExtractor提取有关cargo的信息，
     * 并使用CargoIndex将这些信息索引到系统中它通常在接收到新cargo或更新cargo信息时被调用
     *
     * @param repositoryPath         仓库路径，表示cargo存储的位置
     * @param cargoMetadataExtractor cargo元数据提取器，用于从cargo中提取必要的信息
     * @param index                  索引，用于将提取的信息编入索引以便于搜索和检索
     */
    private void indexEventAsSystem(RepositoryPath repositoryPath, CargoMetadataExtractor cargoMetadataExtractor, CargoIndex index) {
        try {
            switch (index.getType()) {
                case ADD:
                    addCrate(repositoryPath, index,cargoMetadataExtractor);
                    return;
                case REINDEX:
                    reindexPackage(repositoryPath, index.getPackageName(), cargoMetadataExtractor,index);
                    return;
                case YANK:
                case DELETE:
                    reindexPackage(repositoryPath, getPackageName(index.getPackageName()), cargoMetadataExtractor,index);
                    return;
                case CALCULATE_METADATA:
                    calculateAndWriteAttributes(repositoryPath, cargoMetadataExtractor, repositoryPath.getArtifactPath());
                    return;
            }
            log.warn("No index action determined");
        } catch (Exception e) {
            log.error("Error occurred during event processing: {}", e.getMessage());
            log.debug("Error occurred during event processing:", e);
            throw new RuntimeException(e);
        }
    }

    @Nonnull
    private String getPackageName(@Nonnull String fileName) {
        int idx = fileName.lastIndexOf('-');
        return (idx > 0) ? fileName.substring(0, idx) : fileName;
    }

    /**
     * 向仓库路径添加cargo，并提取cargo的元数据信息
     *
     * @param repositoryPath         仓库路径，表示cargo将被添加到的地点
     * @param cargoMetadataExtractor cargo元数据提取器，用于提取cargo的元数据信息
     * @throws Exception 如果添加cargo或提取元数据过程中发生错误，则抛出异常
     */
    private void addCrate(RepositoryPath repositoryPath, CargoIndex index,CargoMetadataExtractor cargoMetadataExtractor) throws Exception {
        String path = repositoryPath.getPath();
        log.debug("Extracting Cargo metadata for repo '{}/{}' on path '{}'", repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), path);
        //Artifact artifact = artifactService.findArtifact(repositoryPath, false);
        String name = index.getPackageName();
        if (name == null) {
            name = getPackageName(repositoryPath.getFileName().toString());
            calculateAndWriteAttributes(repositoryPath, cargoMetadataExtractor, path);
        }
        reindexPackage(repositoryPath, name, cargoMetadataExtractor, index);
    }

    /**
     * 根据cargo元数据提取器计算属性，并将这些属性写入到指定的仓库路径中
     * 此方法主要用于处理元数据的提取和持久化，以确保仓库中的cargo信息是最新的
     *
     * @param repositoryPath         仓库路径，表示cargo元数据将被写入的位置
     * @param cargoMetadataExtractor cargo元数据提取器，用于计算cargo的属性
     * @param path                   字符串路径，通常与仓库路径相匹配，用于确认或验证元数据的写入位置
     * @throws Exception 如果在计算属性或写入过程中发生任何错误，将抛出此异常
     */
    private void calculateAndWriteAttributes(RepositoryPath repositoryPath, CargoMetadataExtractor cargoMetadataExtractor, String path) throws Exception {
        CargoMetadata metadata = cargoMetadataExtractor.extract(repositoryPath);
        log.debug("Indexing Cargo metadata for repo '{}/{}' on path '{}'", repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), path);
        writeAttributes(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), path, metadata);
        String metadataFilePath = CargoUtil.getLongMetadataFilePath(path);
        RepositoryPath artifactPath = artifactResolutionService.resolvePath(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), metadataFilePath);
        if (artifactPath == null || !Files.exists(artifactPath)) {
            RepositoryPath metadataPath = repositoryPathResolver.resolve(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), path);
            CargoUtil.writeLongMetadata(metadata, metadataPath, artifactManagementService);
        }
    }

    /**
     * 重新索引指定包
     * <p>
     * 此方法负责对给定仓库路径下的特定包进行重新索引操作它使用货物元数据提取器来处理包的元数据，
     * 并确保包的最新信息被正确地索引和记录
     *
     * @param repositoryPath         仓库路径，指示需要重新索引的包所在的位置
     * @param packageName            包名，指定需要重新索引的包
     * @param cargoMetadataExtractor 货物元数据提取器，用于提取和处理包的元数据
     * @throws Exception 如果重新索引过程中发生错误，将抛出异常
     */
    private void reindexPackage(RepositoryPath repositoryPath, String packageName, CargoMetadataExtractor cargoMetadataExtractor,CargoIndex index) throws Exception {
        List<RepositoryPath> fileList = new ArrayList<>();
        RepositoryPath artifactPath = artifactResolutionService.resolvePath(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), "crates/"+packageName);
        Files.walkFileTree(artifactPath, new SimpleFileVisitor<>() {
            @Override
            public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) {
                // 在这里可以处理目录（如果需要的话）
                return FileVisitResult.CONTINUE;
            }

            @Override
            public FileVisitResult visitFile(Path filePath, BasicFileAttributes attrs) {
                if (!isHiddenFile(filePath) &&
                        filePath.getFileName().toString().startsWith(packageName) &&
                        filePath.getFileName().toString().endsWith(".crate")) {
                    fileList.add((RepositoryPath) filePath);
                }
                return FileVisitResult.CONTINUE;
            }

            @Override
            public FileVisitResult visitFileFailed(Path file, IOException exc) {
                // 处理无法访问的文件
                log.error("访问{}文件失败: {}", file.toString(), exc.getMessage());
                return FileVisitResult.CONTINUE;
            }
        });


        StringBuilder cargoIndex = new StringBuilder();
        for (RepositoryPath filePath : fileList) {
            reindexPackageVersion(cargoMetadataExtractor, cargoIndex, filePath,index);
        }
        writeIndexLine(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), packageName, cargoIndex);
    }

    /**
     * 重新索引包版本信息
     * 此方法的目的是利用cargo元数据提取器从特定的仓库路径中提取信息，并更新cargo索引
     * 它会处理与特定包版本相关的元数据，并将这些数据追加到现有的索引中
     *
     * @param cargoMetadataExtractor cargo元数据提取器，用于提取包版本信息
     * @param cargoIndex             cargo索引的字符串构建器，用于追加新的索引信息
     * @param repositoryPath         仓库路径，指向需要重新索引的包版本的位置
     * @throws Exception 如果在提取元数据或更新索引过程中发生错误，则抛出异常
     */
    private void reindexPackageVersion(CargoMetadataExtractor cargoMetadataExtractor, StringBuilder cargoIndex, RepositoryPath repositoryPath,CargoIndex index) throws Exception {
        CargoMetadata metadata;
        String versionPath = repositoryPath.getPath();
        String metadataJsonPath = CargoUtil.getLongMetadataFilePath(versionPath);
        if (!Files.exists(repositoryPath)) {
            metadata = cargoMetadataExtractor.extract(repositoryPath);
            log.debug("Writing '{}' file for repo '{}/{}'", metadataJsonPath, repositoryPath.getStorageId(), repositoryPath.getRepositoryId());
            CargoUtil.writeLongMetadata(metadata, repositoryPath, artifactManagementService);
        } else {
            metadata = CargoUtil.attributesMapToMetadata( cargoMetadataExtractor, repositoryPath, () -> getReadLongMetadataHandler(repositoryPath));
        }
        for (CargoDependencyMetadata dependency : metadata.getDeps()){
            if (dependency.getRegistry() == null ){
                dependency.setRegistry(index.getRegistry());
            }
        }
        boolean yanked = getYanked(repositoryPath,index.getType());
        cargoIndex.append(CargoUtil.generateIndexLine(metadata, Files.readString(Path.of(repositoryPath.getTarget() + ".sha256")), yanked));
        cargoIndex.append(System.lineSeparator());
    }

    /**
     * 将指定存储和仓库中的包信息写入索引行。
     * 该方法用于更新cargo索引，以便更方便地跟踪和定位包。
     *
     * @param storageId    存储的唯一标识符，用于指定包的位置。
     * @param repositoryId 仓库的唯一标识符，与storageId结合使用以确定包的确切位置。
     * @param packageName  包的名称，用于标识要索引的包。
     * @param cargoIndex   包含cargo索引信息的StringBuilder对象，将被更新以包含新的包信息。
     * @throws Exception 如果在索引更新过程中发生错误，则抛出异常。
     */
    private void writeIndexLine(String storageId, String repositoryId, String packageName, StringBuilder cargoIndex) throws Exception {
        String indexPath = CargoUtil.calculateIndexPath(packageName);
        String data = cargoIndex.toString();
        log.debug("Indexing Crate internally");
        ByteArrayInputStream content = new ByteArrayInputStream(data.getBytes());
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, "index/" + indexPath);
        artifactManagementService.validateAndStore(repositoryPath, content);
    }

    //private boolean isInternalIndex(RepoWorkContext context) {
    //    return (Boolean.TRUE == context.getParam("internalIndex"));
    //}

    /**
     * 获取读取长元数据的处理器
     * 该方法用于处理在特定仓库路径下读取长类型元数据的需求
     * 主要作用是根据路径信息确定如何解析和返回长类型元数据对象
     *
     * @param repositoryPath 仓库路径，表示元数据在仓库中的位置
     * @return CargoLongMetadata 返回一个长类型元数据对象，该对象包含了从指定路径读取的元数据信息
     */
    public CargoMetadata.CargoLongMetadata getReadLongMetadataHandler(RepositoryPath repositoryPath) {
        String filePath = CargoUtil.getLongMetadataFilePath(repositoryPath.getPath());
        CargoMetadata.CargoLongMetadata cargoLongMetadata = null;
        try {
            if (Files.exists(repositoryPath)) {
                cargoLongMetadata = (CargoMetadata.CargoLongMetadata) (new ObjectMapper()).readValue(Files.readAllBytes(repositoryPath), CargoMetadata.CargoLongMetadata.class);
            }
        } catch (IOException e) {
            log.error("Unable to read {} for {}: {}", new Object[]{filePath, e.getMessage()});
            log.debug("", e);
        }
        return cargoLongMetadata;
    }

    /**
     * 将cargo元数据写入指定存储位置
     * 此方法负责将与cargo相关的元数据信息写入到指定的存储位置中，
     * 它需要存储ID、仓库ID、路径信息以及cargo元数据对象
     *
     * @param storageId    存储标识符，用于指定存储的位置
     * @param repositoryId 仓库标识符，用于指定仓库
     * @param path         文件路径，指定文件在仓库中的位置
     * @param metadata     cargo元数据，包含cargo的相关信息和属性
     * @throws Exception 如果写入过程中发生任何错误，则抛出异常
     */
    private void writeAttributes(String storageId, String repositoryId, String path, CargoMetadata metadata) throws Exception {
        Map<String, String> attributes = CargoUtil.metadataToAttributesMap(metadata);
        RepositoryPath metadataPath = repositoryPathResolver.resolve(storageId, repositoryId, CargoUtil.getLongMetadataFilePath(path));
        artifactManagementService.store(metadataPath, new ByteArrayInputStream(CargoUtil.getMapper().writeValueAsBytes(attributes)));
    }

    /**
     * 判断文件是否为隐藏文件
     *
     * @param filePath 文件路径
     * @return true表示为隐藏文件，false表示不是隐藏文件
     */
    public boolean isHiddenFile(Path filePath) {
        if (filePath.getFileName().toString().startsWith(".")) {
            return true;
        }
        return filePath.getFileName().toString().endsWith(".metadata")
                || filePath.getFileName().toString().endsWith(".md5")
                || filePath.getFileName().toString().endsWith(".sha1")
                || filePath.getFileName().toString().endsWith(".sha256")
                || filePath.getFileName().toString().endsWith(".sm3");
    }

    /**
     * 获取yanked状态
     *
     * @param repositoryPath 仓库路径
     * @param type           事件类型
     * @return yanked状态
     */
    public boolean getYanked(RepositoryPath repositoryPath, CargoIndex.EventType type) {
        String path = repositoryPath.getPath();
        if (path.contains(CargoConstants.CRATE_SUFFIX)) {
            path = String.join("/", CargoConstants.METADATA_DIRECTORY, path.replace(CargoConstants.CRATE_SUFFIX, CargoConstants.LONG_METADATA_FILE_SUFFIX));
        }
        boolean yanked = false;
        if (type.equals(CargoIndex.EventType.YANK)) {
            yanked = true;
        } else {
            try {
                RepositoryPath jsonPath = artifactResolutionService.resolvePath(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), path);
                if (jsonPath != null && Files.exists(jsonPath)) {
                    CargoMetadata.CargoLongMetadata cargoLongMetadata = getReadLongMetadataHandler(jsonPath);

                    if (cargoLongMetadata != null) {
                        yanked = cargoLongMetadata.getYanked();
                    }
                }
            } catch (IOException e) {
                log.error("Unable to read {} for {}: {}", path, e.getMessage());
            }
        }
        return yanked;
    }
}
