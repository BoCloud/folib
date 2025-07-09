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
package com.folib.controllers.layout.cargo;


import com.folib.artifact.coordinates.CargoCoordinates;
import com.folib.controllers.BaseArtifactController;
import com.folib.extractor.CargoIndex;
import com.folib.extractor.CargoMetadataIndexer;
import com.folib.model.CargoMetadata;
import com.folib.model.CargoSearchEntriesModel;
import com.folib.model.CargoSearchModel;
import com.folib.model.publish.CargoPublishRes;
import com.folib.model.req.PublishRequest;
import com.folib.providers.io.RepositoryPath;

import com.folib.storage.repository.Repository;
import com.folib.utils.CargoConstants;
import com.folib.utils.CargoUtil;
import com.folib.utils.CollectionUtils;
import com.folib.web.LayoutReqMapping;
import com.folib.web.RepoMapping;
import lombok.extern.slf4j.Slf4j;


import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import javax.inject.Inject;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import javax.ws.rs.*;


import java.io.ByteArrayInputStream;

import java.io.IOException;
import java.io.InputStream;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

@Slf4j
@RestController
@LayoutReqMapping(CargoCoordinates.LAYOUT_NAME)
public class CargoArtifactController extends BaseArtifactController {


    @Inject
    private CargoMetadataIndexer configurationIndexer;

    private static final String FORBIDDEN_MESSAGE_TEMPLATE = "Operation failed for path '%s'";

    private static final Pattern NAME_AND_VERSION = Pattern.compile("(?<name>.+)-(?<version>\\d+\\.\\d+\\.\\d+(-.+)?(\\+.+)?)\\.crate");


    /**
     * 获取仓库信息
     *
     * @return Response
     */
    @GetMapping(value = "/{storageId}/{repositoryId}", produces = "application/json")
    public ResponseEntity<?> repoInfo() {
        return ResponseEntity.ok().build();
    }


    /**
     * 下载索引文件
     *
     * @param pkg
     * @return
     */
    //@PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @GetMapping(value = "{storageId}/{repositoryId}/index/{pkg:.+}")
    public void downloadIndex(@RepoMapping Repository repository,
                              @RequestHeader HttpHeaders httpHeaders,
                              @PathVariable("storageId") String storageId,
                              @PathVariable("repositoryId") String repositoryId,
                              @PathVariable("pkg") String pkg,
                              HttpServletRequest request,
                              HttpServletResponse response) throws Exception {
        String path = CargoConstants.CONFIG_FILE.equals(pkg) ? pkg : ("index/" + pkg);
        String ifNoneMatch = null;
        if (CargoConstants.CONFIG_FILE.equals(pkg)) {
            response.setHeader("Content-Disposition", String.format("attachment; filename=\"%s\"", CargoConstants.CONFIG_FILE));
        } else {
            ifNoneMatch = httpHeaders.getFirst("If-None-Match");
        }
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
        repositoryPath.setTargetUrl(pkg);
        repositoryPath = artifactResolutionService.resolvePath(repositoryPath);
        if (repositoryPath != null && repositoryPath.getTarget() != null) {
            String sha1 = Files.readString(Path.of(repositoryPath.getTarget().toString() + ".sha1"));
            response.setHeader("ETag", sha1);
            response.setHeader("X-Checksum-Sha1", sha1);
            response.setHeader("X-Checksum-Sha256", Files.readString(Path.of(repositoryPath.getTarget().toString() + ".sha256")));
            response.setHeader("X-Checksum-Md5", Files.readString(Path.of(repositoryPath.getTarget().toString() + ".md5")));
            response.setHeader("Content-Disposition", String.format("attachment; filename=\"%s\"", repositoryPath.getPath()));
            if (ifNoneMatch != null && ifNoneMatch.equals(sha1)) {
                response.setStatus(HttpStatus.NOT_MODIFIED.value());
                return;
            }
        }

        vulnerabilityBlock(repositoryPath);
        provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);


    }

    /**
     * 下载包
     *
     * @param packageName 包名
     * @param version     版本
     * @return
     */
    //@PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @GetMapping(value = "/{storageId}/{repositoryId}/api/v1/crates/{packageName}/{version}/download")
    public void download(@RepoMapping Repository repository,
                         @RequestHeader HttpHeaders httpHeaders,
                         @PathVariable("storageId") String storageId,
                         @PathVariable("repositoryId") String repositoryId,
                         @PathVariable("packageName") String packageName,
                         @PathVariable("version") String version,
                         HttpServletRequest request,
                         HttpServletResponse response) {

        String path = CargoUtil.buildCratePath(packageName, version);
        try {
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
            repositoryPath.setTargetUrl(String.format("%s/%s/download", packageName, version));
            repositoryPath = artifactResolutionService.resolvePath(repositoryPath);
            vulnerabilityBlock(repositoryPath);
            provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
        } catch (Exception e) {
            log.error("Failed to download package '{}' version '{}:{}", new Object[]{packageName, version, e.getMessage()});
            log.debug("Failed to download package '{}' version '{}:", new Object[]{packageName, version, e});
            throw new NotFoundException("Unable to download crate", e);
        }
    }

    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @GetMapping(value = {"/{storageId}/{repositoryId}/config.json", "/{storageId}/{repositoryId}/crates/{path:.+}"})
    public void downloadPackage(@RepoMapping Repository repository,
                                @RequestHeader HttpHeaders httpHeaders,
                                @PathVariable("storageId") String storageId,
                                @PathVariable("repositoryId") String repositoryId,
                                @PathVariable(name = "path", required = false) String path,
                                HttpServletRequest request,
                                HttpServletResponse response) throws Exception {
        path = path == null ? "config.json" : "crates/" + path;
        RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, path);
        vulnerabilityBlock(repositoryPath);
        provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
    }


    /**
     * 上传包
     */
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @PutMapping(value = "/{storageId}/{repositoryId}/api/v1/crates/new")
    public ResponseEntity<?> publish(@RepoMapping Repository repository,
                                     @PathVariable("repositoryId") String repositoryId,
                                     HttpServletRequest request) {
        List<String> errors = new ArrayList<>();
        final String storageId = repository.getStorage().getId();
        try (InputStream inputStream = request.getInputStream()) {
            PublishRequest publishRequest = extractPublishRequest(inputStream);
            String cratePath = CargoUtil.buildCratePath(publishRequest.getMetadata().getName(), publishRequest.getMetadata().getVers());
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, cratePath);
            if (repositoryPath == null) {
                return ResponseEntity.status(HttpStatus.NOT_FOUND).body(String.format("[%s] directory does not exist ", cratePath));
            }
            //写入制品文件
            artifactManagementService.store(repositoryPath, new ByteArrayInputStream(publishRequest.getCrateFile()));
            //写入索引文件
            CargoIndex cargoIndex = null;
            if(Objects.nonNull(publishRequest.getMetadata()) && !CollectionUtils.isNullOrEmpty(publishRequest.getMetadata().getDeps()) ){
                cargoIndex = new CargoIndex(publishRequest.getMetadata().getName(), CargoIndex.EventType.ADD, publishRequest.getMetadata().getDeps().get(0).getRegistry());
            }else {
                cargoIndex =  new CargoIndex(publishRequest.getMetadata().getName(), CargoIndex.EventType.ADD);
            }
            configurationIndexer.indexAsSystem(repositoryPath, cargoIndex);
            String metadataFilePath = CargoUtil.getLongMetadataFilePath(cratePath);
            //写入长索引文件
            RepositoryPath path = repositoryPathResolver.resolve(storageId, repositoryId, metadataFilePath);
            CargoUtil.writeLongMetadata(publishRequest.getMetadata(), path, artifactManagementService);
        } catch (Exception e) {
            errors.add(String.format("Failed to publish with error '%s'", new Object[]{e.getMessage()}));
            log.error("Failed to publish into repo '{}/{}'", storageId, repositoryId, e);
            return ResponseEntity.ok(new CargoPublishRes(List.of(), List.of(), errors));
        }
        return ResponseEntity.ok((new CargoPublishRes(List.of(), List.of(), errors)));
    }

    /**
     * 撤销包
     *
     * @param name
     * @param version
     * @return
     */
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @DeleteMapping(value = "/{storageId}/{repositoryId}/api/v1/crates/{crateName}/{version}/yank")
    public ResponseEntity<?> yank(@PathVariable("storageId") String storageId,
                                  @PathVariable("repositoryId") String repositoryId,
                                  @PathVariable("crateName") String name,
                                  @PathVariable("version") String version) throws Exception {

        String cratePath = CargoUtil.buildCratePath(name, version);
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, cratePath);
        if (repositoryPath == null || !Files.exists(repositoryPath)) {
            return createForbiddenResponse(cratePath);
        }
        configurationIndexer.indexAsSystem(repositoryPath, new CargoIndex(name, CargoIndex.EventType.YANK));
        String metadataFilePath = CargoUtil.getLongMetadataFilePath(cratePath);
        //写入长索引文件
        RepositoryPath path = repositoryPathResolver.resolve(storageId, repositoryId, metadataFilePath);
        if (path != null && Files.exists(path)) {
            CargoMetadata.CargoLongMetadata cargoLongMetadata = configurationIndexer.getReadLongMetadataHandler(path);
            if (cargoLongMetadata != null) {
                cargoLongMetadata.setYanked(true);
                CargoUtil.writeLongMetadata(cargoLongMetadata, path, artifactManagementService);
            }
        }
        log.debug("Yank executed for repo {}/{} and path {}", storageId, repositoryId, cratePath);
        return ResponseEntity.ok("{\"ok\":true}");
    }


    /**
     * 取消删除包
     *
     * @param name
     * @param version
     * @return
     */
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @PutMapping(value = "/{storageId}/{repositoryId}/api/v1/crates/{crateName}/{version}/unyank")
    public ResponseEntity<?> unyank(@PathVariable("storageId") String storageId,
                                    @PathVariable("repositoryId") String repositoryId,
                                    @PathVariable("crateName") String name,
                                    @PathVariable("version") String version) throws Exception {
        String cratePath = CargoUtil.buildCratePath(name, version);
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, cratePath);
        if (repositoryPath == null || !Files.exists(repositoryPath)) {
            return createForbiddenResponse(cratePath);
        }
        String metadataFilePath = CargoUtil.getLongMetadataFilePath(cratePath);
        //写入长索引文件
        RepositoryPath path = repositoryPathResolver.resolve(storageId, repositoryId, metadataFilePath);
        if (path != null && Files.exists(path)) {
            CargoMetadata.CargoLongMetadata cargoLongMetadata = configurationIndexer.getReadLongMetadataHandler(path);
            if (cargoLongMetadata != null) {
                cargoLongMetadata.setYanked(false);
                CargoUtil.writeLongMetadata(cargoLongMetadata, path, artifactManagementService);
            }
        }
        configurationIndexer.indexAsSystem(repositoryPath, new CargoIndex(name, CargoIndex.EventType.DELETE));
        log.debug("Unyank executed for repo {}/{}  and path {}", storageId, repositoryId, cratePath);
        return ResponseEntity.ok("{\"ok\":true}");
    }

    /**
     * 搜索包
     *
     * @param query
     * @param perPage
     * @return
     */
    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @GetMapping(value = "/{storageId}/{repositoryId}/api/v1/crates")
    public ResponseEntity<?> search(@PathVariable("storageId") String storageId,
                                    @PathVariable("repositoryId") String repositoryId,
                                    @RequestParam(name = "q", required = false) String query,
                                    @RequestParam(name = "per_page", required = false, defaultValue = "10") Integer perPage) throws Exception {

        perPage = CargoUtil.getSearchPerPage(perPage);
        if (query == null || query.isEmpty()) {
            return ResponseEntity.ok(new CargoSearchModel(new ArrayList<>(), 0));
        }

        List<String> indexNames = searchIndex(storageId, repositoryId, query);
        if (indexNames.isEmpty()) {
            return ResponseEntity.ok(new CargoSearchModel(new ArrayList<>(), 0));
        }
        List<CargoSearchEntriesModel> list = indexNames.stream().limit(perPage).map(indexName -> {
            try {
                return getArtifacts(storageId, repositoryId, indexName);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }).collect(Collectors.toList());
        return ResponseEntity.ok(new CargoSearchModel(list, indexNames.size()));
    }

    /**
     * 搜索包
     *
     * @param storageId    存储空间ID
     * @param repositoryId 存储库ID
     * @param query        包名
     * @return 包列表
     */
    public List<String> searchIndex(String storageId, String repositoryId, String query) throws IOException {
        List<String> indexNames = new ArrayList<>();
        RepositoryPath artifactPath = artifactResolutionService.resolvePath(storageId, repositoryId, CargoConstants.INDEX_PATH);
        Files.walkFileTree(artifactPath, new SimpleFileVisitor<>() {
            @Override
            public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) {
                // 在这里可以处理目录（如果需要的话）
                return FileVisitResult.CONTINUE;
            }

            @Override
            public FileVisitResult visitFile(Path filePath, BasicFileAttributes attrs) {
                if (!isHiddenFile(filePath) &&
                        filePath.getFileName().toString().contains(query)) {
                    indexNames.add(filePath.getFileName().toString());
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
        return indexNames;
    }


    /**
     * 获取包列表
     *
     * @param storageId    存储空间ID
     * @param repositoryId 仓库ID
     * @param artifactName 包名
     */
    public CargoSearchEntriesModel getArtifacts(String storageId, String repositoryId, String artifactName) throws IOException {
        List<RepositoryPath> artifactPaths = new ArrayList<>();
        RepositoryPath artifactPath = artifactResolutionService.resolvePath(storageId, repositoryId, "crates/" + artifactName);
        Files.walkFileTree(artifactPath, new SimpleFileVisitor<>() {
            @Override
            public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) {
                // 在这里可以处理目录（如果需要的话）
                return FileVisitResult.CONTINUE;
            }

            @Override
            public FileVisitResult visitFile(Path filePath, BasicFileAttributes attrs) {
                if (!isHiddenFile(filePath) &&
                        filePath.getFileName().toString().startsWith(artifactName) &&
                        filePath.getFileName().toString().endsWith(".crate")) {
                    artifactPaths.add((RepositoryPath) filePath);
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
        // 使用 Stream API 获取最大值
        Optional<CargoSearchEntriesModel> maxData = artifactPaths.stream().map(item -> {
            return nameAndVersion(item.getPath());
        }).filter(Objects::nonNull).max(CargoSearchEntriesModel::compareVersion);
        return maxData.orElse(null);
    }

    /**
     * 提取发布请求
     *
     * @param inputStream 输入流
     */
    PublishRequest extractPublishRequest(InputStream inputStream) throws IOException {
        String content = new String(inputStream.readNBytes(readLittleEndianInt(inputStream)));
        CargoMetadata metadata = CargoUtil.stringToMetadata(content);
        byte[] crateFile = inputStream.readNBytes(readLittleEndianInt(inputStream));
        return new PublishRequest(metadata, crateFile);
    }

    /**
     * 读取小端整数
     *
     * @param eis 输入流
     */
    private int readLittleEndianInt(InputStream eis) throws IOException {
        return ByteBuffer.wrap(eis.readNBytes(4)).order(ByteOrder.LITTLE_ENDIAN).getInt();
    }

    /**
     * 创建禁止响应
     *
     * @param cratePath path
     * @return 禁止响应
     */
    private ResponseEntity<?> createForbiddenResponse(String cratePath) {
        return ResponseEntity.status(HttpStatus.FORBIDDEN)
                .body(String.format("Operation failed for path '%s'", cratePath));
    }

    /**
     * 判断是否是隐藏文件
     *
     * @param filePath 文件路径
     * @return 是否是隐藏文件
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
     * 提取包名与版本
     *
     * @param path 路径
     * @return 包名与版本
     */
    private CargoSearchEntriesModel nameAndVersion(String path) {
        CargoSearchEntriesModel cargoMetadata = new CargoSearchEntriesModel();
        cargoMetadata.setPath(path);
        int nameIdx = path.lastIndexOf('/');
        if (nameIdx < 0)
            return null;
        String nameAndVersion = path.substring(nameIdx + 1);
        Matcher matcher = NAME_AND_VERSION.matcher(nameAndVersion);
        if (!matcher.find())
            return null;
        cargoMetadata.setName(matcher.group("name"));
        cargoMetadata.setMaxVersion(matcher.group("version"));
        return cargoMetadata;
    }
}