package com.veadan.folib.controllers.adapter.jfrog;

import cn.hutool.json.JSONUtil;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.veadan.folib.artifact.coordinates.DockerArtifactCoordinates;
import com.veadan.folib.components.layout.DockerComponent;
import com.veadan.folib.domain.*;
import com.veadan.folib.domain.adapter.jfrog.*;
import com.veadan.folib.enums.ArtifactFieldTypeEnum;
import com.veadan.folib.enums.ArtifactSearchConditionTypeEnum;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.layout.DockerLayoutProvider;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.schema2.ImageManifest;
import com.veadan.folib.schema2.LayerManifest;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.utils.FileUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import java.io.File;
import java.nio.file.Files;
import java.time.ZoneId;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * @author leipenghui
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
        String repoKey = "repo", pathKey = "path";
        // 提取 "repo", "path" 和 "include" 字段
        String storageId = getDefaultStorageId();
        String repositoryId = findJson.getString(repoKey);
        Storage storage = getStorage(storageId);
        if (Objects.isNull(storage)) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(handlerErrors(null, STORAGE_NOT_FOUND_MESSAGE));
        }
        if (Objects.isNull(storage.getRepository(repositoryId))) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(handlerErrors(null, REPOSITORY_NOT_FOUND_MESSAGE));
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
        ArtifactSearchCondition artifactSearchCondition = ArtifactSearchCondition.builder().storageId(storageId).repositoryId(repositoryId).path("").artifactConditionGroups(artifactConditionGroups).artifactSorts(artifactSorts).build();
        String includePatternText = "\\.include\\((.*?)\\)";
        Pattern includePattern = Pattern.compile(includePatternText);
        Matcher includeMatcher = includePattern.matcher(query);
        if (includeMatcher.find()) {
            String includeText = includeMatcher.group(1);
            String[] includeFieldArray = includeText.split(",");
            List<String> unIncludeList = Lists.newArrayList("\"*\"");
            for (String includeField : includeFieldArray) {
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
        String storageId = getDefaultStorageId(), repositoryId = artifactDockerQuery.getRepoKey(), artifactPath = artifactDockerQuery.getPath();
        String[] imageArr = artifactPath.split("/");
        String name = imageArr[0];
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
        DockerArtifactCoordinates dockerArtifactCoordinates = (DockerArtifactCoordinates) artifact.getArtifactCoordinates();
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
        if (artifact.getArtifactCoordinates() instanceof DockerArtifactCoordinates) {
            DockerArtifactCoordinates dockerArtifactCoordinates = (DockerArtifactCoordinates) artifact.getArtifactCoordinates();
            name = dockerArtifactCoordinates.getTAG();
        }
        return name;
    }

    private String getPath(Artifact artifact) {
        String path = artifact.getArtifactPath(), name = artifact.getArtifactName();
        if (artifact.getArtifactCoordinates() instanceof DockerArtifactCoordinates) {
            DockerArtifactCoordinates dockerArtifactCoordinates = (DockerArtifactCoordinates) artifact.getArtifactCoordinates();
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
