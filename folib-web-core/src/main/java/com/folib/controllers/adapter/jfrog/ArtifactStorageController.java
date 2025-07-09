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
package com.folib.controllers.adapter.jfrog;

import com.alibaba.fastjson.JSONObject;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.folib.controllers.adapter.jfrog.res.FileRes;
import com.folib.domain.Artifact;
import com.folib.domain.adapter.jfrog.ArtifactStorageInfo;
import com.folib.enums.ArtifactMetadataEnum;

import com.folib.forms.artifact.ArtifactMetadataForm;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.services.ArtifactWebService;
import com.folib.storage.ArtifactStorageException;
import com.folib.storage.Storage;
import com.folib.util.RepositoryPathUtil;
import io.swagger.annotations.*;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.activation.MimetypesFileTypeMap;
import javax.inject.Inject;
import jakarta.servlet.http.HttpServletRequest;
import java.io.IOException;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.*;

import static org.springframework.http.HttpStatus.NOT_FOUND;

/**
 * @author veadan
 */
@Slf4j
@RestController
@RequestMapping("/artifactory/api/storage")
//@PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
@Api(description = "JFrog存储", tags = "JFrog存储")
public class ArtifactStorageController extends JFrogBaseController {

    private static final String NOT_FOUND_MESSAGE = "No properties could be found.";

    private static final String ARTIFACT_NOT_FOUND_MESSAGE = "The artifact was not found.";

    private static final String STORAGE_NOT_FOUND_MESSAGE = "The storage was not found.";

    private static final String REPOSITORY_NOT_FOUND_MESSAGE = "The repository was not found.";

    private static final String PROPERTIES_VALUE_CANNOT_BE_EMPTY = "Properties value cannot be empty.";

    private static final String PATH_NOT_FOUND_MESSAGE = "The path was not found.";

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private ArtifactWebService artifactWebService;

    @ApiOperation(value = "查询元数据")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @RequestMapping(value = {"/{repositoryId}/{artifactPath:.+}"}, method = {RequestMethod.GET})
    public ResponseEntity<?> itemProperties(@PathVariable("repositoryId") String repositoryId, @PathVariable("artifactPath") String artifactPath,
                                            @RequestParam(value = "properties", required = false) String properties, HttpServletRequest request) throws Exception {
        String storageId = getDefaultStorageId(repositoryId);
        Storage storage = getStorage(storageId);
        if (Objects.isNull(storage)) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(handlerErrors(null, STORAGE_NOT_FOUND_MESSAGE));
        }
        if (Objects.isNull(storage.getRepository(repositoryId))) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(handlerErrors(null, REPOSITORY_NOT_FOUND_MESSAGE));
        }

        if (Collections.list(request.getParameterNames()).stream().anyMatch("list"::equals)) {
            return getStorageFileList(repositoryId, storageId, artifactPath, request);
        }
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
        String propertiesKey = "properties";
        boolean hasPropertiesKey = request.getParameterMap().containsKey(propertiesKey);
        String metadata = artifactWebService.getMetadata(storageId, repositoryId, artifactPath);
        if (hasPropertiesKey && (StringUtils.isBlank(metadata) || "{}".equals(metadata))) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(handlerErrors(null, null));
        }
        ArtifactStorageInfo artifactStorageInfo = ArtifactStorageInfo.builder().uri(request.getRequestURL().toString()).build();
        if (hasPropertiesKey && StringUtils.isNotBlank(metadata)) {
            List<String> propertyList = null, valueList = null;
            if (StringUtils.isNotBlank(properties)) {
                propertyList = Arrays.asList(properties.split(","));
            }
            JSONObject metadataJson = JSONObject.parseObject(metadata);
            if (CollectionUtils.isNotEmpty(propertyList) && metadataJson.keySet().stream().noneMatch(propertyList::contains)) {
                return ResponseEntity.status(HttpStatus.NOT_FOUND).body(handlerErrors(null, null));
            }
            Map<String, Object> propertiesMap = Maps.newLinkedHashMap();
            String value;
            for (String key : metadataJson.keySet()) {
                value = metadataJson.getJSONObject(key).getString("value");
                if (StringUtils.isBlank(value)) {
                    propertiesMap.put(key, "");
                    continue;
                }
                valueList = Arrays.asList(value.split(","));
                if (CollectionUtils.isNotEmpty(propertyList)) {
                    if (propertyList.contains(key)) {
                        propertiesMap.put(key, valueList);
                    }
                } else {
                    propertiesMap.put(key, valueList);
                }
                artifactStorageInfo.setProperties(propertiesMap);
            }
        }
        if (!hasPropertiesKey) {
            Artifact artifact = null;
            if (RepositoryFiles.isArtifact(repositoryPath)) {
                artifact = findArtifact(storageId, repositoryId, artifactPath);
                if (Objects.nonNull(artifact)) {
                    artifactStorageInfo.setCreatedBy(artifact.getCreatedBy());
                    artifactStorageInfo.setModifiedBy(artifact.getUpdatedBy());
                    artifactStorageInfo.setCreated(Date.from(artifact.getCreated().atZone(ZoneId.of("Asia/Shanghai")).toOffsetDateTime().toInstant()));
                    artifactStorageInfo.setLastModified(Date.from(artifact.getLastUpdated().atZone(ZoneId.of("Asia/Shanghai")).toOffsetDateTime().toInstant()));
                    artifactStorageInfo.setDownloadUri(String.format("%s/%s", getBaseUrl(storageId, repositoryId), artifactPath));
                    MimetypesFileTypeMap mimetypesFileTypeMap = new MimetypesFileTypeMap();
                    String mimeType = mimetypesFileTypeMap.getContentType(artifact.getArtifactPath());
                    artifactStorageInfo.setMimeType(mimeType);
                    artifactStorageInfo.setSize(Objects.nonNull(artifact.getSizeInBytes()) ? artifact.getSizeInBytes().toString() : "0");
                    Map<String, String> checksumsMap = artifact.getChecksums();
                    artifactStorageInfo.setChecksums(replaceKey(checksumsMap));
                    artifactStorageInfo.setOriginalChecksums(artifactStorageInfo.getChecksums());
                }
            } else if (Files.isDirectory(repositoryPath)) {
                artifactStorageInfo.setCreated(RepositoryPathUtil.getFileCreationDate(repositoryPath));
                artifactStorageInfo.setLastModified(RepositoryPathUtil.getFileLastModifiedDate(repositoryPath));
            }
            artifactStorageInfo.setRepo(String.format("%s/%s", storageId, repositoryId));
            artifactStorageInfo.setPath("/" + artifactPath);
            artifactStorageInfo.setLastUpdated(artifactStorageInfo.getLastModified());
        }
        return ResponseEntity.ok(artifactStorageInfo);
    }

    @ApiOperation(value = "设置元数据")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PutMapping(value = {"/{repositoryId}/{artifactPath:.+}"})
    public ResponseEntity<Object> setItemProperties(@PathVariable("repositoryId") String repositoryId, @PathVariable("artifactPath") String artifactPath,
                                                    @RequestParam(value = "properties", required = false) String properties, @RequestParam(value = "recursive", required = false, defaultValue = "1") Boolean recursive, HttpServletRequest request) throws Exception {

        String storageId = getDefaultStorageId(repositoryId);
        Storage storage = getStorage(storageId);
        if (StringUtils.isBlank(properties)) {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(handlerErrors(HttpStatus.BAD_REQUEST.value(), PROPERTIES_VALUE_CANNOT_BE_EMPTY));
        }
        if (Objects.isNull(storage)) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(handlerErrors(null, STORAGE_NOT_FOUND_MESSAGE));
        }
        if (Objects.isNull(storage.getRepository(repositoryId))) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(handlerErrors(null, REPOSITORY_NOT_FOUND_MESSAGE));
        }
        String splitSemicolon = ";", splitVerticalLine = "|", splitVerticalLineEncode = "\\|", splitSlashSemicolon = "\\;", splitSlashSemicolonEncode = "\\\\;", splitComma = ",";
        List<String> propertyList = Arrays.asList(properties.split(splitSemicolon)), itemPropertyList = null, propertyGroupSplitList = null;
        if (properties.contains(splitVerticalLine)) {
            propertyList = Arrays.asList(properties.split(splitVerticalLineEncode));
        }
        List<ArtifactMetadataForm> artifactMetadataFormList = Lists.newArrayList();
        ArtifactMetadataForm artifactMetadataForm = null;
        String metadataKey, metadataValue;
        if (CollectionUtils.isNotEmpty(propertyList)) {
            for (String propertyGroup : propertyList) {
                propertyGroupSplitList = Arrays.asList(propertyGroup.split("="));
                metadataKey = propertyGroupSplitList.get(0);
                if (propertyGroupSplitList.size() >= 2) {
                    metadataValue = propertyGroupSplitList.get(1);
                } else {
                    metadataValue = "";
                }
                artifactMetadataForm = ArtifactMetadataForm.builder().storageId(storageId).repositoryId(repositoryId).artifactPath(artifactPath).type(ArtifactMetadataEnum.STRING.toString()).key(metadataKey).viewShow(1).recursive(recursive).build();
                itemPropertyList = Arrays.asList(metadataValue.split(splitComma));
                if (metadataValue.contains(splitSlashSemicolon)) {
                    itemPropertyList = Arrays.asList(metadataValue.split(splitSlashSemicolonEncode));
                }
                if (CollectionUtils.isNotEmpty(itemPropertyList)) {
                    artifactMetadataForm.setValue(String.join(",", itemPropertyList));
                }
                artifactMetadataFormList.add(artifactMetadataForm);
            }
            artifactWebService.batchArtifactMetadata(artifactMetadataFormList);
        }
        return ResponseEntity.status(HttpStatus.NO_CONTENT.value()).body("");
    }

    /**
     * 替换key
     *
     * @param checksumsMap 源map
     * @return 新map
     */
    private Map<String, String> replaceKey(Map<String, String> checksumsMap) {
        Map<String, String> newChecksumsMap = Maps.newLinkedHashMap();
        Map<String, String> replacementMap = Maps.newHashMap();
        replacementMap.put("SHA-1", "sha1");
        replacementMap.put("SHA-256", "sha256");
        replacementMap.put("SHA-512", "sha512");
        replacementMap.put("MD5", "md5");
        for (Map.Entry<String, String> entry : checksumsMap.entrySet()) {
            String key = entry.getKey();
            key = replacementMap.getOrDefault(key, key);
            newChecksumsMap.put(key, entry.getValue());
        }
        return newChecksumsMap;
    }

    /**
     * 获取文件夹列表
     *
     * @param storageId    存储ID
     * @param repositoryId 仓库ID
     * @param artifactPath 文件夹路径
     * @return 文件夹列表
     */
    public ResponseEntity<?> getStorageFileList(String repositoryId,
                                                String storageId,
                                                String artifactPath,
                                                HttpServletRequest request
    ) {
        boolean deep = "1".equals(request.getParameter("deep"));

        int depth = Integer.parseInt(request.getParameter("depth"));
        boolean listFolders = "1".equals(request.getParameter("listFolders"));
        boolean mdTimestamps = "1".equals(request.getParameter("mdTimestamps"));
        boolean includeRootPath = "1".equals(request.getParameter("includeRootPath"));

        Map<String, Object> result = new HashMap<>();
        try {
            RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, artifactPath);

            //String folderPath;
            //RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, folderPath);
            if (!Files.exists(repositoryPath)) {
                return ResponseEntity.status(HttpStatus.NOT_FOUND).body(handlerErrors(null, PATH_NOT_FOUND_MESSAGE));
            }

            List<FileRes> files = listDirectory(repositoryPath, deep, depth, listFolders, mdTimestamps, includeRootPath);
            result.put("files", files);
            result.put("created", LocalDateTime.now().toString());
            result.put("uri", String.format("%s://%s:%s%s", request.getScheme(), request.getServerName(), request.getServerPort(), request.getRequestURI()));
        } catch (Exception e) {
            logger.error("获取文件列表失败", e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
        }
        return ResponseEntity.ok(result);

    }

    /**
     * 获取文件夹列表
     *
     * @param rootPath        文件夹路径
     * @param deep            是否递归
     * @param depth           递归深度
     * @param listFolders     是否返回文件夹列表
     * @param mdTimestamps    是否返回元数据
     * @param includeRootPath 是否包含根路径
     * @return 文件夹列表
     */
    public List<FileRes> listDirectory(RepositoryPath rootPath,
                                       boolean deep,
                                       int depth,
                                       boolean listFolders,
                                       boolean mdTimestamps,
                                       boolean includeRootPath) throws IOException {
        //Path startPath = Paths.get(rootPath);
        List<FileRes> result = new ArrayList<>();

        Files.walkFileTree(rootPath, new SimpleFileVisitor<Path>() {
            int currentDepth = 0;

            // 控制递归深度
            @Override
            public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) throws IOException {
                if (currentDepth > depth && !deep) {
                    return FileVisitResult.SKIP_SUBTREE;
                }
                if (listFolders && (includeRootPath || !((RepositoryPath) dir).getPath().equals(((RepositoryPath) rootPath).getPath()))) {
                    if (!dir.toString().endsWith(".foLibrary-metadata")) {
                        // true 表示这是一个文件夹
                        result.add(formatOutput(dir, attrs, true));
                    }
                }
                currentDepth++;
                return FileVisitResult.CONTINUE;
            }

            @Override
            public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
                if (!file.toString().endsWith(".md5")
                        && !file.toString().endsWith(".sha1")
                        && !file.toString().endsWith(".sha256")
                        && !file.toString().endsWith(".metadata")
                        && !file.toString().endsWith(".sm3")
                        && !file.toString().contains(".foLibrary-metadata")) {
                    // false 表示这是一个文件
                    result.add(formatOutput(file, attrs, false));
                }
                return FileVisitResult.CONTINUE;
            }

            @Override
            public FileVisitResult postVisitDirectory(Path dir, IOException exc) throws IOException {
                currentDepth--;
                return FileVisitResult.CONTINUE;
            }

            // 格式化输出
            private FileRes formatOutput(Path path, BasicFileAttributes attrs, boolean isFolder) throws IOException {
                FileRes fileRes = new FileRes();
                fileRes.setUri(((RepositoryPath) path).getPath());
                if (isFolder) {
                    fileRes.setFolder("true");
                } else {
                    fileRes.setFolder("false")
                            .setSize(String.valueOf(attrs.size()));
                    Path filePath = Paths.get(path.toString() + ".sha1");
                    fileRes.setSha1(Files.readString(filePath));
                }
                fileRes.setLastModified(String.valueOf(attrs.lastModifiedTime()));
                if (mdTimestamps) {
                    Map<String, String> mdTimestamps = new HashMap<>();
                    mdTimestamps.put("properties", String.valueOf(attrs.lastModifiedTime()));
                    fileRes.setMdTimestamps(mdTimestamps);
                }

                return fileRes;
            }
        });

        return result;
    }

    @ApiOperation(value = "删除制品或目录")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @DeleteMapping(value = {"/{repositoryId}/{artifactPath:.+}"})
    public ResponseEntity<?> delete(@PathVariable String repositoryId,
                                    @ApiParam(value = "Whether to use force delete")
                                    @RequestParam(defaultValue = "false", name = "force", required = false) boolean force,
                                    @PathVariable String artifactPath) throws IOException {
        final String storageId = getDefaultStorageId(repositoryId);
        Storage storage = getStorage(storageId);
        if (Objects.isNull(storage)) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(handlerErrors(null, STORAGE_NOT_FOUND_MESSAGE));
        }
        if (Objects.isNull(storage.getRepository(repositoryId))) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(handlerErrors(null, REPOSITORY_NOT_FOUND_MESSAGE));
        }
        logger.info("Deleting {}:{}/{}...", storageId, repositoryId, artifactPath);
        try {
            final RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
            if (!Files.exists(repositoryPath)) {
                return ResponseEntity.status(NOT_FOUND)
                        .body("The specified path does not exist!");
            }
            artifactManagementService.delete(repositoryPath, force);
        } catch (ArtifactStorageException e) {
            logger.error(e.getMessage(), e);
            return ResponseEntity.status(HttpStatus.BAD_REQUEST)
                    .body(e.getMessage());
        }

        return ResponseEntity.ok("The artifact was deleted.");
    }

    @ApiOperation(value = "设置自定义元数据")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PatchMapping(value = {"/{repositoryId}/{artifactPath:.+}"})
    public ResponseEntity<Object> setItemCustomProperties(@PathVariable("repositoryId") String repositoryId,
                                                          @PathVariable("artifactPath") String artifactPath,
                                                          @RequestParam(value = "recursive", required = false, defaultValue = "1") Boolean recursive,
                                                          @RequestParam(required = false) Map<String, String> customProperties,
                                                          HttpServletRequest request) throws Exception {

        String storageId = getDefaultStorageId(repositoryId);
        Storage storage = getStorage(storageId);
        if (customProperties == null) {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(handlerErrors(HttpStatus.BAD_REQUEST.value(), PROPERTIES_VALUE_CANNOT_BE_EMPTY));
        }
        if (Objects.isNull(storage)) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(handlerErrors(null, STORAGE_NOT_FOUND_MESSAGE));
        }
        if (Objects.isNull(storage.getRepository(repositoryId))) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(handlerErrors(null, REPOSITORY_NOT_FOUND_MESSAGE));
        }
        List<ArtifactMetadataForm> artifactMetadataFormList = Lists.newArrayList();
        // 遍历所有接收到的参数
        for (Map.Entry<String, String> entry : customProperties.entrySet()) {
            ArtifactMetadataForm artifactMetadataForm = ArtifactMetadataForm.builder()
                    .storageId(storageId)
                    .repositoryId(repositoryId)
                    .artifactPath(artifactPath)
                    .type(ArtifactMetadataEnum.STRING.toString())
                    .key(entry.getKey())
                    .value(entry.getValue())
                    .viewShow(1)
                    .recursive(recursive)
                    .build();
            artifactMetadataFormList.add(artifactMetadataForm);
        }
        artifactWebService.batchArtifactMetadata(artifactMetadataFormList);
        return ResponseEntity.status(HttpStatus.NO_CONTENT.value()).body("");
    }

}
