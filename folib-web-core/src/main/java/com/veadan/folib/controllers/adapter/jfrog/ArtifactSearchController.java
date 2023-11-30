package com.veadan.folib.controllers.adapter.jfrog;

import com.alibaba.fastjson.JSONObject;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.veadan.folib.artifact.coordinates.DockerArtifactCoordinates;
import com.veadan.folib.components.layout.DockerComponent;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.ArtifactMetadataCondition;
import com.veadan.folib.domain.ArtifactNameCondition;
import com.veadan.folib.domain.ArtifactSearchCondition;
import com.veadan.folib.domain.adapter.jfrog.*;
import com.veadan.folib.enums.ArtifactSearchConditionTypeEnum;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
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
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
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
        Map<String, String> fields = Maps.newHashMap();
        List<String> includeFields = Lists.newArrayList();
        String path = null, repoKey = "repo";
        // 使用正则表达式匹配查询字符串中的字段和条件
        String fieldPatternText = "\"([^\"]+)\":\\s*\"([^\"]+)\"";
        Pattern fieldPattern = Pattern.compile(fieldPatternText);
        Matcher fieldMatcher = fieldPattern.matcher(query);
        while (fieldMatcher.find()) {
            String fieldName = fieldMatcher.group(1);
            String fieldValue = fieldMatcher.group(2);
            fields.put(fieldName, fieldValue);
        }
        // 提取 "repo", "path" 和 "include" 字段
        String storageId = getDefaultStorageId();
        String repositoryId = fields.get(repoKey);
        Storage storage = getStorage(storageId);
        if (Objects.isNull(storage)) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(handlerErrors(null, STORAGE_NOT_FOUND_MESSAGE));
        }
        if (Objects.isNull(storage.getRepository(repositoryId))) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(handlerErrors(null, REPOSITORY_NOT_FOUND_MESSAGE));
        }
        String pathPatternText = "\"path\":\\s*\\{\\s*\"\\$(match)\":\\s*\"(.*?)\"\\s*}";
        Pattern pathPattern = Pattern.compile(pathPatternText);
        Matcher pathMatcher = pathPattern.matcher(query);
        if (pathMatcher.find()) {
            path = pathMatcher.group(2);
        }
        List<ArtifactNameCondition> artifactNameConditions = Lists.newArrayList();
        List<ArtifactMetadataCondition> artifactMetadataConditions = Lists.newArrayList();

        ArtifactSearchCondition artifactSearchCondition = ArtifactSearchCondition.builder().storageId(storageId).repositoryId(repositoryId).path(path).artifactNameConditions(artifactNameConditions).artifactMetadataConditions(artifactMetadataConditions).build();
        String namePatternText = "\"name\"\\s*:\\s*\\{\\s*\"\\$(eq|ne|match|nmatch)\":\\s*\"(.*?)\"\\s*}";
        Pattern namePattern = Pattern.compile(namePatternText);
        Matcher nameMatcher = namePattern.matcher(query);
        ArtifactNameCondition artifactNameCondition = null;
        while (nameMatcher.find()) {
            String operator = nameMatcher.group(1);
            String value = nameMatcher.group(2);
            artifactNameCondition = ArtifactNameCondition.builder().artifactSearchConditionTypeEnum(ArtifactSearchConditionTypeEnum.queryTypeEnum(operator)).searchValue(value).build();
            artifactNameConditions.add(artifactNameCondition);
        }
        String includePatternText = "\\.include\\((.*?)\\)";
        Pattern includePattern = Pattern.compile(includePatternText);
        Matcher includeMatcher = includePattern.matcher(query);
        if (includeMatcher.find()) {
            String includeText = includeMatcher.group(1);
            String[] includeFieldArray = includeText.split(",");
            for (String includeField : includeFieldArray) {
                includeFields.add(includeField.trim().replaceAll("\"", ""));
            }
        }
        // 动态获取字段名
        String metadataPatternText = "\"@(\\w+)\"\\s*:\\s*\\{\\s*\"\\$(eq|ne|match|nmatch)\":\\s*\"(.*?)\"\\s*}";
        Pattern metadataPattern = Pattern.compile(metadataPatternText);
        Matcher metadataMatcher = metadataPattern.matcher(query);
        ArtifactMetadataCondition artifactMetadataCondition = null;
        while (metadataMatcher.find()) {
            String metadataKey = metadataMatcher.group(1);
            String operator = metadataMatcher.group(2);
            String metadataValue = metadataMatcher.group(3);
            artifactMetadataCondition = ArtifactMetadataCondition.builder().artifactSearchConditionTypeEnum(ArtifactSearchConditionTypeEnum.queryTypeEnum(operator)).medataKey(metadataKey).medataValue(metadataValue).build();
            artifactMetadataConditions.add(artifactMetadataCondition);
        }
        List<ArtifactSearchInfo> results = Lists.newArrayList();
        int zero = 0;
        ArtifactSearchRange range = ArtifactSearchRange.builder().startPos(zero).endPos(zero).total(zero).build();
        ArtifactSearchResult artifactSearchResult = ArtifactSearchResult.builder().results(results).range(range).build();
        Page<Artifact> page = artifactRepository.findMatchingByAql(getPageable(query), artifactSearchCondition);
        if (Objects.nonNull(page)) {
            List<String> metadataKeyList = includeFields.stream().filter(item -> item.startsWith("@")).map(item -> item.replace("@", "")).collect(Collectors.toList());
            range.setStartPos(page.getPageable().getPageNumber() + 1);
            range.setEndPos(page.getPageable().getPageSize());
            range.setTotal(Long.valueOf(page.getTotalElements()).intValue());
            if (CollectionUtils.isNotEmpty(page.getContent())) {
                ArtifactSearchInfo artifactSearchInfo = null;
                for (Artifact artifact : page.getContent()) {
                    artifactSearchInfo = filterObjectByProperties(artifact, includeFields);
                    filterMetadata(artifactSearchInfo, artifact, metadataKeyList);
                    results.add(artifactSearchInfo);
                }
            }
        }
        return ResponseEntity.ok(artifactSearchResult);
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
        RepositoryPath manifestConfigPath = repositoryPathResolver.resolve(storageId, repositoryId, name + "/blobs/" + configDigest);
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

    private Pageable getPageable(String query) {
        Pageable pageable = null;
        Integer page = null, limit = null;
        String regex = "limit\\((\\d+)\\)";
        limit = getInteger(query, regex);
        if (Objects.isNull(limit)) {
            limit = 100;
        }
        regex = "offset\\((\\d+)\\)";
        page = getInteger(query, regex);
        if (Objects.isNull(page)) {
            page = 1;
        }
        if (page == 1) {
            pageable = PageRequest.of(page, limit).first();
        } else {
            pageable = PageRequest.of(page, limit).previous();
        }
        return pageable;
    }

    private Integer getInteger(String query, String regex) {
        Integer result = null;
        Pattern pattern = Pattern.compile(regex);
        // 创建匹配器
        Matcher matcher = pattern.matcher(query);
        // 查找匹配项
        if (matcher.find()) {
            // 提取 limit 值
            result = Integer.parseInt(matcher.group(1));
        }
        return result;
    }

    private ArtifactSearchInfo filterObjectByProperties(Artifact artifact, List<String> propertyList) {
        ArtifactSearchInfo artifactSearchInfo = new ArtifactSearchInfo();
        if (CollectionUtils.isEmpty(propertyList)) {
            artifactSearchInfo.setRepo(String.format("%s/%s", artifact.getStorageId(), artifact.getRepositoryId()));
            artifactSearchInfo.setName(artifact.getArtifactName());
            artifactSearchInfo.setPath(getPath(artifact.getArtifactPath(), artifact.getArtifactName()));
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
                    artifactSearchInfo.setPath(getPath(artifact.getArtifactPath(), artifact.getArtifactName()));
                    break;
                case "name":
                    artifactSearchInfo.setName(artifact.getArtifactName());
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

    private String getPath(String path, String name) {
        if (StringUtils.isBlank(path)) {
            return "";
        }
        if (path.equals(name)) {
            path = ".";
        } else {
            path = path.substring(0, path.indexOf(name) - 1);
        }
        return path;
    }

}
