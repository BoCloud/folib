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

import cn.hutool.json.JSONUtil;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.folib.domain.*;
import com.folib.domain.adapter.jfrog.*;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.folib.artifact.coordinates.DockerCoordinates;
import com.folib.components.layout.DockerComponent;
import com.folib.constant.GlobalConstants;
import com.folib.enums.ArtifactFieldTypeEnum;
import com.folib.enums.ArtifactSearchConditionTypeEnum;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.providers.layout.DockerLayoutProvider;
import com.folib.repositories.ArtifactRepository;
import com.folib.schema2.ImageManifest;
import com.folib.schema2.LayerManifest;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;
import com.folib.utils.FileUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.inject.Inject;
import jakarta.servlet.http.HttpServletRequest;
import java.io.File;
import java.nio.file.Files;
import java.time.ZoneId;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * @author veadan
 */
@Slf4j
@RestController
//@PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
@Api(description = "JFrog搜索", tags = "JFrog搜索")
public class ArtifactSearchController extends JFrogBaseController {

    private static final String NOT_FOUND_MESSAGE = "No properties could be found.";

    private static final String ARTIFACT_NOT_FOUND_MESSAGE = "The artifact was not found.";

    private static final String STORAGE_NOT_FOUND_MESSAGE = "The storage was not found.";

    private static final String REPOSITORY_NOT_FOUND_MESSAGE = "The repository was not found.";

    private static final String DOCKER_MANIFEST_NOT_FOUND_MESSAGE = "Unable to find Docker manifest under '%s'.";

    private static final String INCLUDE_CANNOT_BE_EMPTY = "Include cannot be empty.";

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private DockerComponent dockerComponent;

    @Inject
    private ArtifactRepository artifactRepository;

    @ApiOperation(value = "JFrog搜索")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PostMapping(value = {"/artifactory/api/search/aql"})
    public ResponseEntity<Object> aqlSearch(@RequestBody String query, HttpServletRequest request) throws Exception {
        List<ArtifactSearchInfo> results = Lists.newArrayList();
        long zero = 0L;
        ArtifactSearchRange range = ArtifactSearchRange.builder().startPos(zero).endPos(zero).total(zero).build();
        ArtifactSearchResult artifactSearchResult = ArtifactSearchResult.builder().results(results).range(range).build();
        JSONObject findJson;
        String findPatternText = "items\\.find\\(([^)]+)\\)";
        // 使用正则表达式匹配 items.find() 括号内的内容
        Pattern pattern = Pattern.compile(findPatternText, Pattern.DOTALL);
        Matcher matcher = pattern.matcher(query);
        if (matcher.find()) {
            String contentInsideFind = matcher.group(1).trim();
            log.info("Content inside items.find() [{}]" + contentInsideFind);
            findJson = JSONObject.parseObject(contentInsideFind);
        } else {
            log.warn("No match found for items.find() [{}]", query);
            return ResponseEntity.ok(artifactSearchResult);
        }
        List<String> includeFields = Lists.newArrayList();
        String repoKey = "repo", pathKey = "path", typeKey = "type", storageId = "";
        String repositoryId = findJson.getString(repoKey);
        if (StringUtils.isNotBlank(repositoryId)) {
            // 提取 "repo", "path" 和 "include" 字段
            storageId = getDefaultStorageId(repositoryId);
            Storage storage = getStorage(storageId);
            if (Objects.isNull(storage)) {
                return ResponseEntity.status(HttpStatus.NOT_FOUND).body(handlerErrors(null, STORAGE_NOT_FOUND_MESSAGE));
            }
            if (Objects.isNull(storage.getRepository(repositoryId))) {
                return ResponseEntity.status(HttpStatus.NOT_FOUND).body(handlerErrors(null, REPOSITORY_NOT_FOUND_MESSAGE));
            }
        }
        List<ArtifactConditionGroup> artifactConditionGroups = Lists.newArrayList();
        ArtifactConditionGroup orArtifactConditionGroup = ArtifactConditionGroup.builder().artifactSearchConditionTypeEnum(ArtifactSearchConditionTypeEnum.OR)
                .artifactConditions(Lists.newArrayList()).artifactMetadataConditions(Lists.newArrayList()).artifactNameConditions(Lists.newArrayList()).build();
        artifactConditionGroups.add(orArtifactConditionGroup);
        ArtifactConditionGroup andArtifactConditionGroup = ArtifactConditionGroup.builder().artifactSearchConditionTypeEnum(ArtifactSearchConditionTypeEnum.AND)
                .artifactConditions(Lists.newArrayList()).artifactMetadataConditions(Lists.newArrayList()).artifactNameConditions(Lists.newArrayList()).build();
        artifactConditionGroups.add(andArtifactConditionGroup);
        boolean flag = findJson.containsKey(ArtifactSearchConditionTypeEnum.OR.getSource()) || findJson.containsKey(ArtifactSearchConditionTypeEnum.AND.getSource());
        for (String key : findJson.keySet()) {
            if (flag) {
                String str = findJson.getString(key);
                if (StringUtils.isBlank(str) || !JSONUtil.isJson(str)) {
                    continue;
                }
                Optional<ArtifactConditionGroup> artifactConditionGroupOptional = artifactConditionGroups.stream().filter(item -> key.equals(item.getArtifactSearchConditionTypeEnum().getSource())).findFirst();
                if (artifactConditionGroupOptional.isPresent()) {
                    ArtifactConditionGroup artifactConditionGroup = artifactConditionGroupOptional.get();
                    JSONArray jsonArray = findJson.getJSONArray(key);
                    for (int i = 0; i < jsonArray.size(); i++) {
                        JSONObject json = jsonArray.getJSONObject(i);
                        for (String itemKey : json.keySet()) {
                            handle(pathKey, itemKey, null, json.getJSONObject(itemKey), artifactConditionGroup);
                        }
                    }
                }
            } else {
                handle(pathKey, key, findJson.getString(key), null, andArtifactConditionGroup);
            }
        }
        List<ArtifactSort> artifactSorts = Lists.newArrayList();
        String sortPatternText = "\\.sort\\(([^)]+)\\)";
        Pattern sortPattern = Pattern.compile(sortPatternText, Pattern.DOTALL);
        Matcher sortMatcher = sortPattern.matcher(query);
        if (sortMatcher.find()) {
            String sortStr = sortMatcher.group(1).trim();
            JSONObject sortJson = JSONObject.parseObject(sortStr);
            ArtifactFieldTypeEnum artifactFieldTypeEnum = null;
            for (String sortKey : sortJson.keySet()) {
                List<String> keyList = Lists.newArrayList();
                for (String key : sortJson.getJSONArray(sortKey).toJavaList(String.class)) {
                    artifactFieldTypeEnum = ArtifactFieldTypeEnum.queryTypeEnum(key);
                    if (Objects.isNull(artifactFieldTypeEnum)) {
                        continue;
                    }
                    keyList.add(artifactFieldTypeEnum.getFolibary());
                }
                artifactSorts.add(ArtifactSort.builder().order(sortKey.replace("$", "")).keyList(keyList).build());
            }
        }
        String type = findJson.getString(typeKey);
        ArtifactSearchCondition artifactSearchCondition = ArtifactSearchCondition.builder().storageId(storageId).repositoryId(repositoryId).path("").type(type).artifactConditionGroups(artifactConditionGroups).artifactSorts(artifactSorts).build();
        String includePatternText = "\\.include\\((.*?)\\)";
        Pattern includePattern = Pattern.compile(includePatternText);
        Matcher includeMatcher = includePattern.matcher(query);
        if (includeMatcher.find()) {
            String includeText = includeMatcher.group(1);
            if (StringUtils.isBlank(includeText)) {
                return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(handlerErrors(HttpStatus.BAD_REQUEST.value(), INCLUDE_CANNOT_BE_EMPTY));
            }
            String none = "\"\"";
            String[] includeFieldArray = includeText.split(",");
            List<String> unIncludeList = Lists.newArrayList("\"*\"");
            for (String includeField : includeFieldArray) {
                if (StringUtils.isBlank(includeField) || none.equals(includeField)) {
                    return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(handlerErrors(HttpStatus.BAD_REQUEST.value(), INCLUDE_CANNOT_BE_EMPTY));
                }
                if (unIncludeList.stream().anyMatch(includeField::equals)) {
                    continue;
                }
                includeFields.add(includeField.trim().replaceAll("\"", ""));
            }
        }
        ArtifactSearch<Artifact> artifactArtifactSearch = artifactRepository.findMatchingByAql(getPageable(query), artifactSearchCondition);
        if (Objects.nonNull(artifactArtifactSearch)) {
            List<String> metadataKeyList = includeFields.stream().filter(item -> item.startsWith("@") || item.contains("property")).map(item -> item.replace("@", "")).collect(Collectors.toList());
            range.setStartPos(artifactArtifactSearch.getRange().getStartPos());
            range.setTotal(artifactArtifactSearch.getRange().getTotal());
            range.setEndPos(artifactArtifactSearch.getRange().getEndPos());
            range.setLimit(artifactArtifactSearch.getRange().getLimit());
            if (CollectionUtils.isNotEmpty(artifactArtifactSearch.getResults())) {
                ArtifactSearchInfo artifactSearchInfo = null;
                for (Artifact artifact : artifactArtifactSearch.getResults()) {
                    artifactSearchInfo = filterObjectByProperties(artifact, includeFields);
                    filterMetadata(artifactSearchInfo, artifact, metadataKeyList);
                    results.add(artifactSearchInfo);
                }
            }
        }
        return ResponseEntity.ok(artifactSearchResult);
    }

    private String handle(String pathKey, String key, String content, JSONObject json, ArtifactConditionGroup artifactConditionGroup) {
        String path = "";
        if (StringUtils.isNotBlank(content) && JSONUtil.isJson(content)) {
            json = JSONObject.parseObject(content);
        } else if (StringUtils.isNotBlank(content) && !JSONUtil.isJson(content)) {
            json = new JSONObject();
            json.put("$eq", content);
        }
        if (Objects.isNull(json) || json.isEmpty()) {
            return path;
        }
        if (key.startsWith("@")) {
            for (String conditionKey : json.keySet()) {
                //元数据
                artifactConditionGroup.getArtifactMetadataConditions().add(ArtifactMetadataCondition.builder().medataKey(key.replace("@", ""))
                        .artifactSearchConditionTypeEnum(ArtifactSearchConditionTypeEnum.queryTypeEnumBySource(conditionKey)).medataValue(json.getString(conditionKey)).build());
            }
        } else if ("name".equalsIgnoreCase(key)) {
            for (String itemKey : json.keySet()) {
                artifactConditionGroup.getArtifactNameConditions().add(ArtifactNameCondition.builder()
                        .artifactSearchConditionTypeEnum(ArtifactSearchConditionTypeEnum.queryTypeEnumBySource(itemKey)).searchValue(json.getString(itemKey)).build());
            }
        } else {
            ArtifactFieldTypeEnum artifactFieldTypeEnum = null;
            for (String itemKey : json.keySet()) {
                artifactFieldTypeEnum = ArtifactFieldTypeEnum.queryTypeEnum(key);
                if (Objects.isNull(artifactFieldTypeEnum)) {
                    continue;
                }
                artifactConditionGroup.getArtifactConditions().add(ArtifactCondition.builder()
                        .artifactSearchConditionTypeEnum(ArtifactSearchConditionTypeEnum.queryTypeEnumBySource(itemKey)).searchKey(artifactFieldTypeEnum.getFolibary()).searchValue(json.getString(itemKey)).build());
            }
        }
        return path;
    }


    @ApiOperation(value = "JFrog搜索")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PostMapping(value = {"/ui/api/v1/ui/views/dockerv2"})
    public ResponseEntity<Object> dockerv2(@RequestBody ArtifactDockerQuery artifactDockerQuery, HttpServletRequest request) throws Exception {
        String repositoryId = artifactDockerQuery.getRepoKey(), artifactPath = artifactDockerQuery.getPath(), storageId = getDefaultStorageId(repositoryId);
        Storage storage = getStorage(storageId);
        if (Objects.isNull(storage)) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(handlerErrors(null, STORAGE_NOT_FOUND_MESSAGE));
        }
        if (Objects.isNull(storage.getRepository(repositoryId))) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(handlerErrors(null, REPOSITORY_NOT_FOUND_MESSAGE));
        }
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
        List<ImageManifest> imageManifestList = dockerComponent.getImageManifests(repositoryPath);
        if (CollectionUtils.isEmpty(imageManifestList) || imageManifestList.size() != 1) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(handlerErrors(null, String.format(DOCKER_MANIFEST_NOT_FOUND_MESSAGE, artifactPath)));
        }
        ImageManifest imageManifest = imageManifestList.get(0);
        String configDigest = imageManifest.getConfig().getDigest();
        RepositoryPath manifestConfigPath = repositoryPathResolver.resolve(storageId, repositoryId, DockerLayoutProvider.BLOBS + File.separator + configDigest);
        String manifestConfigString = Files.readString(manifestConfigPath);
        ManifestConfig manifestConfig = JSONObject.parseObject(manifestConfigString, ManifestConfig.class);
        String exposedPortsKey = "ExposedPorts", volumesKey = "Volumes", labelsKey = "Labels";
        List<String> ports = null, volumes = null, labels = null;
        if (manifestConfig.getConfig().containsKey(exposedPortsKey) && Objects.nonNull(manifestConfig.getConfig().getJSONObject(exposedPortsKey)) && !manifestConfig.getConfig().getJSONObject(exposedPortsKey).isEmpty()) {
            ports = Lists.newArrayList(manifestConfig.getConfig().getJSONObject(exposedPortsKey).keySet());
        }
        if (manifestConfig.getConfig().containsKey(volumesKey) && Objects.nonNull(manifestConfig.getConfig().getJSONObject(volumesKey)) && !manifestConfig.getConfig().getJSONObject(volumesKey).isEmpty()) {
            volumes = Lists.newArrayList(manifestConfig.getConfig().getJSONObject(volumesKey).keySet());
        }
        if (manifestConfig.getConfig().containsKey(labelsKey) && Objects.nonNull(manifestConfig.getConfig().getJSONObject(labelsKey)) && !manifestConfig.getConfig().getJSONObject(labelsKey).isEmpty()) {
            labels = Lists.newArrayList(manifestConfig.getConfig().getJSONObject(labelsKey).keySet());
        }
        Artifact artifact = findArtifact(storageId, repositoryId, artifactPath);
        if (Objects.isNull(artifact)) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(handlerErrors(null, String.format(DOCKER_MANIFEST_NOT_FOUND_MESSAGE, artifactPath)));
        }
        DockerCoordinates dockerArtifactCoordinates = (DockerCoordinates) artifact.getArtifactCoordinates();
        Long totalSize = imageManifest.getLayers().stream().filter(item -> Objects.nonNull(item.getSize())).mapToLong(LayerManifest::getSize).sum();
        DockerTagInfo dockerTagInfo = DockerTagInfo.builder().title(dockerArtifactCoordinates.getId()).digest(imageManifest.getDigest()).totalSize(FileUtils.formatSize(totalSize)).totalSizeLong(totalSize)
                .ports(ports).volumes(volumes).labels(labels).build();
        List<DockerBlobsInfo> dockerBlobsInfoList = Lists.newArrayList();
        List<DockHistory> dockHistoryList = manifestConfig.getHistory();
        DockerBlobsInfo dockerBlobsInfo = null;
        List<LayerManifest> layerManifestList = imageManifest.getLayers();
        List<String> runList = Lists.newArrayList("/bin/sh", "/bin/bash", "/usr/bin/env bash", "/usr/bin/bash"), commandList = Lists.newArrayList("RUN", "COPY", "ADD");
        int index = 0;
        for (DockHistory dockHistory : dockHistoryList) {
            String size = "0 B", digest = null, command = "", commandText = "", nop = "#(nop)", run = "RUN", createBy = "";
            createBy = dockHistory.getCreatedBy();
            if (createBy.contains(nop)) {
                command = getCommand(createBy);
                if (StringUtils.isNotBlank(command)) {
                    commandText = createBy.substring(createBy.indexOf(command));
                    if (commandList.stream().anyMatch(command::equals) && index < layerManifestList.size()) {
                        commandText = createBy.substring(createBy.indexOf(command) + command.length() + 1);
                        LayerManifest layerManifest = layerManifestList.get(index);
                        digest = layerManifest.getDigest();
                        size = FileUtils.formatSize(layerManifest.getSize());
                        index = index + 1;
                    }
                }
            } else {
                commandText = createBy;
                if (runList.stream().anyMatch(createBy::startsWith) && index < layerManifestList.size()) {
                    LayerManifest layerManifest = layerManifestList.get(index);
                    digest = layerManifest.getDigest();
                    size = FileUtils.formatSize(layerManifest.getSize());
                    index = index + 1;
                }
                command = run;
            }
            dockerBlobsInfo = DockerBlobsInfo.builder().size(size).created(dockHistory.getCreated()).command(command).commandText(commandText).digest(digest).build();
            dockerBlobsInfoList.add(dockerBlobsInfo);
        }
        Collections.reverse(dockerBlobsInfoList);
        DockerImageInfo dockerImageInfo = DockerImageInfo.builder().tagInfo(dockerTagInfo).blobsInfo(dockerBlobsInfoList).build();
        return ResponseEntity.ok(dockerImageInfo);
    }

    @ApiOperation(value = "JFrog Pattern搜索")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @GetMapping(value = {"/artifactory/api/search/pattern"})
    public ResponseEntity<Object> searchPattern(@RequestParam("pattern") String pattern, HttpServletRequest request) throws Exception {
        if (StringUtils.isBlank(pattern) || !pattern.contains(GlobalConstants.COLON)) {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(handlerErrors(null, GlobalConstants.REQUEST_PARAMS_ERROR));
        }
        String[] params = pattern.split(GlobalConstants.COLON);
        String repositoryId = params[0], storageId = getDefaultStorageId(repositoryId), query = pattern.replace(repositoryId + GlobalConstants.COLON, "");
        Storage storage = getStorage(storageId);
        if (Objects.isNull(storage)) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(handlerErrors(null, STORAGE_NOT_FOUND_MESSAGE));
        }
        Repository repository = storage.getRepository(repositoryId);
        if (Objects.isNull(repository)) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(handlerErrors(null, REPOSITORY_NOT_FOUND_MESSAGE));
        }
        ArtifactPatternSearch artifactPatternSearch = ArtifactPatternSearch.builder().repoUri(getArtifactoryRepositoryUrl(repository, "")).sourcePattern(query).build();
        List<ArtifactConditionGroup> artifactConditionGroups = Lists.newArrayList();
        ArtifactConditionGroup andArtifactConditionGroup = ArtifactConditionGroup.builder().artifactSearchConditionTypeEnum(ArtifactSearchConditionTypeEnum.AND)
                .artifactConditions(Lists.newArrayList()).artifactMetadataConditions(Lists.newArrayList()).artifactNameConditions(Lists.newArrayList()).build();
        artifactConditionGroups.add(andArtifactConditionGroup);
        andArtifactConditionGroup.getArtifactConditions().add(ArtifactCondition.builder()
                .artifactSearchConditionTypeEnum(ArtifactSearchConditionTypeEnum.MATCH).searchKey(ArtifactFieldTypeEnum.PATH.getFolibary()).searchValue(query).searchValueSuffixEnd(true).build());
        ArtifactSearchCondition artifactSearchCondition = ArtifactSearchCondition.builder().storageId(storageId).repositoryId(repositoryId).path("").type("").artifactConditionGroups(artifactConditionGroups).build();
        ArtifactSearch<Artifact> artifactSearch = artifactRepository.findMatchingByAql(ArtifactPage.builder().offset(0L).limit(9999L).build(), artifactSearchCondition);
        if (Objects.nonNull(artifactSearch)) {
            artifactPatternSearch.setFiles(Optional.ofNullable(artifactSearch.getResults()).orElse(Lists.newArrayList()).stream().map(Artifact::getArtifactPath).collect(Collectors.toList()));
        }
        return ResponseEntity.ok(artifactPatternSearch);
    }

    /**
     * 解析command
     *
     * @param text 源文本
     * @return 结果
     */
    private String getCommand(String text) {
        String regex = ".*\\(nop\\)\\s+(\\w+).*";
        Pattern pattern = Pattern.compile(regex);
        Matcher matcher = pattern.matcher(text);
        if (matcher.find()) {
            return matcher.group(1);
        } else {
            return "";
        }
    }

    private ArtifactPage getPageable(String query) {
        ArtifactPage artifactPage = ArtifactPage.builder().build();
        Long offset = null, limit = null;
        String regex = "limit\\((\\d+)\\)";
        limit = getLong(query, regex);
        if (Objects.isNull(limit)) {
            limit = 100L;
        }
        artifactPage.setLimit(limit);
        regex = "offset\\((\\d+)\\)";
        offset = getLong(query, regex);
        if (Objects.isNull(offset)) {
            offset = 0L;
        }
        artifactPage.setOffset(offset);
        return artifactPage;
    }

    private Long getLong(String query, String regex) {
        Long result = null;
        Pattern pattern = Pattern.compile(regex);
        // 创建匹配器
        Matcher matcher = pattern.matcher(query);
        // 查找匹配项
        if (matcher.find()) {
            // 提取 limit 值
            result = Long.parseLong(matcher.group(1));
        }
        return result;
    }

    private ArtifactSearchInfo filterObjectByProperties(Artifact artifact, List<String> propertyList) {
        ArtifactSearchInfo artifactSearchInfo = new ArtifactSearchInfo();
        if (CollectionUtils.isEmpty(propertyList) || (CollectionUtils.isNotEmpty(propertyList) && propertyList.contains("property"))) {
            artifactSearchInfo.setRepo(String.format("%s/%s", artifact.getStorageId(), artifact.getRepositoryId()));
            artifactSearchInfo.setName(getName(artifact));
            artifactSearchInfo.setPath(getPath(artifact));
            artifactSearchInfo.setCreated(Date.from(artifact.getCreated().atZone(ZoneId.of("Asia/Shanghai")).toOffsetDateTime().toInstant()));
            artifactSearchInfo.setCreatedBy(artifact.getCreatedBy());
            artifactSearchInfo.setModified(Date.from(artifact.getLastUpdated().atZone(ZoneId.of("Asia/Shanghai")).toOffsetDateTime().toInstant()));
            artifactSearchInfo.setModifiedBy(artifact.getUpdatedBy());
            artifactSearchInfo.setUpdated(Date.from(artifact.getLastUpdated().atZone(ZoneId.of("Asia/Shanghai")).toOffsetDateTime().toInstant()));
            artifactSearchInfo.setType("file");
            artifactSearchInfo.setSize(artifact.getSizeInBytes());
            return artifactSearchInfo;
        }
        for (String property : propertyList) {
            switch (property) {
                case "repo":
                    artifactSearchInfo.setRepo(String.format("%s/%s", artifact.getStorageId(), artifact.getRepositoryId()));
                    break;
                case "path":
                    artifactSearchInfo.setPath(getPath(artifact));
                    break;
                case "name":
                    artifactSearchInfo.setName(getName(artifact));
                    break;
                case "created":
                    artifactSearchInfo.setCreated(Date.from(artifact.getCreated().atZone(ZoneId.of("Asia/Shanghai")).toOffsetDateTime().toInstant()));
                    break;
                case "createdBy":
                    artifactSearchInfo.setCreatedBy(artifact.getCreatedBy());
                    break;
                case "modified":
                    artifactSearchInfo.setModified(Date.from(artifact.getLastUpdated().atZone(ZoneId.of("Asia/Shanghai")).toOffsetDateTime().toInstant()));
                    break;
                case "modifiedBy":
                    artifactSearchInfo.setModifiedBy(artifact.getUpdatedBy());
                    break;
                case "updated":
                    artifactSearchInfo.setUpdated(Date.from(artifact.getLastUpdated().atZone(ZoneId.of("Asia/Shanghai")).toOffsetDateTime().toInstant()));
                    break;
                case "type":
                    artifactSearchInfo.setType("file");
                    break;
                case "size":
                    artifactSearchInfo.setSize(artifact.getSizeInBytes());
                    break;
                default:
                    // Handle unknown properties or ignore them
                    break;
            }
        }
        return artifactSearchInfo;
    }

    private void filterMetadata(ArtifactSearchInfo artifactSearchInfo, Artifact artifact, List<String> metadataList) {
        if (CollectionUtils.isNotEmpty(metadataList)) {
            if (StringUtils.isNotBlank(artifact.getMetadata())) {
                List<Map<String, Object>> properties = Lists.newArrayList();
                Map<String, Object> map = null;
                JSONObject metadataJson = JSONObject.parseObject(artifact.getMetadata());
                Set<String> keySet = metadataJson.keySet();
                if (metadataList.contains("property")) {
                    metadataList = Lists.newArrayList(keySet);
                }
                String key = "key", valueKey = "value";
                for (String metadataKey : metadataList) {
                    String values = "", value = "";
                    JSONObject valueJson = null;
                    if (keySet.contains(metadataKey)) {
                        map = Maps.newHashMap();
                        map.put(key, metadataKey);
                        values = metadataJson.getString(metadataKey);
                        if (StringUtils.isNotBlank(values)) {
                            valueJson = JSONObject.parseObject(values);
                            value = valueJson.getString("value");
                        }
                        map.put(valueKey, value);
                        properties.add(map);
                    }
                }
                artifactSearchInfo.setProperties(properties);
            }
        }
    }

    private String getName(Artifact artifact) {
        String name = artifact.getArtifactName();
        if (artifact.getArtifactCoordinates() instanceof DockerCoordinates) {
            DockerCoordinates dockerArtifactCoordinates = (DockerCoordinates) artifact.getArtifactCoordinates();
            name = dockerArtifactCoordinates.getTAG();
        }
        return name;
    }

    private String getPath(Artifact artifact) {
        String path = artifact.getArtifactPath(), name = artifact.getArtifactName();
        if (artifact.getArtifactCoordinates() instanceof DockerCoordinates) {
            DockerCoordinates dockerArtifactCoordinates = (DockerCoordinates) artifact.getArtifactCoordinates();
            path = dockerArtifactCoordinates.getName();
        } else {
            if (StringUtils.isBlank(path)) {
                return "";
            }
            if (path.equals(name)) {
                path = ".";
            } else {
                path = path.substring(0, path.indexOf(name) - 1);
            }
        }
        return path;
    }

}
