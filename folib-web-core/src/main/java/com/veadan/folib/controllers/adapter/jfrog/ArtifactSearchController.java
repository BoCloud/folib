package com.veadan.folib.controllers.adapter.jfrog;

import com.alibaba.fastjson.JSONObject;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.veadan.folib.artifact.coordinates.DockerArtifactCoordinates;
import com.veadan.folib.components.layout.DockerComponent;
import com.veadan.folib.controllers.BaseController;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.adapter.jfrog.DockerImageInfo;
import com.veadan.folib.domain.adapter.jfrog.*;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
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
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import java.nio.file.Files;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author leipenghui
 */
@Slf4j
@RestController
@RequestMapping("/artifactory/api/search")
@PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
@Api(description = "JFrog搜索", tags = "JFrog搜索")
public class ArtifactSearchController extends BaseController {

    private static final String NOT_FOUND_MESSAGE = "No properties could be found.";

    private static final String ARTIFACT_NOT_FOUND_MESSAGE = "The artifact was not found.";

    private static final String STORAGE_NOT_FOUND_MESSAGE = "The storage was not found.";

    private static final String REPOSITORY_NOT_FOUND_MESSAGE = "The repository was not found.";

    private static final String DOCKER_MANIFEST_NOT_FOUND_MESSAGE = "Unable to find Docker manifest under '%s'.";

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private DockerComponent dockerComponent;

    @ApiOperation(value = "JFrog搜索")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PostMapping(value = {"/dockerv2"})
    public ResponseEntity<Object> dockerv2(@RequestBody ArtifactDockerQuery artifactDockerQuery, HttpServletRequest request) throws Exception {
        String[] arr = artifactDockerQuery.getRepoKey().split("/");
        String storageId = arr[0], repositoryId = arr[1], artifactPath = artifactDockerQuery.getPath();
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
        DockerArtifactCoordinates dockerArtifactCoordinates = (DockerArtifactCoordinates) artifact.getArtifactCoordinates();
        Long totalSize = imageManifest.getLayers().stream().mapToLong(LayerManifest::getSize).sum();
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
     * 处理错误
     *
     * @param status  状态
     * @param message 消息
     * @return 结果
     */
    private Map<String, Object> handlerErrors(Integer status, String message) {
        Map<String, Object> result = Maps.newHashMap();
        Map<String, Object> resultData = Maps.newHashMap();
        if (Objects.isNull(status)) {
            status = HttpStatus.NOT_FOUND.value();
        }
        resultData.put("status", status);
        if (StringUtils.isBlank(message)) {
            message = NOT_FOUND_MESSAGE;
        }
        resultData.put("message", message);
        result.put("errors", Collections.singletonList(resultData));
        return result;
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

}
