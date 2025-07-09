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
package com.folib.job.tasks.cleanup;

import cn.hutool.json.JSONUtil;
import com.alibaba.fastjson.JSONObject;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.folib.constant.GlobalConstants;
import com.folib.domain.Artifact;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.services.ArtifactManagementService;
import com.folib.util.RepositoryPathUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import jakarta.inject.Inject;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author veadan
 **/
@Slf4j
@Component
public class DockerCleanupArtifactsProvider implements CleanupArtifactsProvider {

    @Inject
    private CleanupArtifactsProviderRegistry cleanupArtifactsProviderRegistry;

    @Inject
    private ArtifactManagementService artifactManagementService;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    private static final Integer ONE = 1;

    private static final Integer ZERO = 0;

    private static final Long MINUS_ONE = -1L;

    private static final String PREFIX = "sha256:";

    @PostConstruct
    @Override
    public void register() {
        cleanupArtifactsProviderRegistry.addProvider("DOCKER", this);
        log.info("Registered cleanup repository cron job '[{}]' with alias '[{}]'.",
                getClass().getCanonicalName(), "DOCKER");
    }

    @Override
    public void cleanup(String storageId, String repositoryId, String path, String storageDay, String storageCondition, Map<String, String> cleanupArtifactPathMap) throws Exception {
        try {
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId);
            String blobs = "blobs", manifest = "manifest", tag = "tag";
            List<String> excludeList = Lists.newArrayList(blobs, manifest);
            List<RepositoryPath> repositoryPathList = RepositoryPathUtil.getDockerImagePaths(repositoryPath);
            if (CollectionUtils.isEmpty(repositoryPathList)) {
                log.info("Repository storageId [{}] repositoryId [{}] not found artifacts", storageId, repositoryId);
                return;
            }
            log.info("Start cleanup artifact job storageId [{}] repositoryId [{}] storageCondition [{}] storageDay [{}] image quantity [{}]", storageId, repositoryId, storageCondition, storageDay, repositoryPathList.size());
            List<Integer> resultList = Lists.newArrayList();
            RepositoryPath imageRepositoryPath;
            for (Path imagePath : repositoryPathList) {
                try {
                    imageRepositoryPath = (RepositoryPath) imagePath;
                    log.info("Cleanup artifact job storageId [{}] repositoryId [{}] storageCondition [{}] storageDay [{}] imagePath [{}]", storageId, repositoryId, storageCondition, storageDay, imageRepositoryPath);
                    if (tag.equalsIgnoreCase(storageCondition)) {
                        //按照tag保留
                        handlerTag(storageId, repositoryId, storageDay, storageCondition, imageRepositoryPath, excludeList, resultList, cleanupArtifactPathMap);
                    } else {
                        //按照天数保留
                        handlerDay(storageId, repositoryId, storageDay, storageCondition, imageRepositoryPath, excludeList, resultList, cleanupArtifactPathMap);
                    }
                } catch (Exception ex) {
                    log.error("Cleanup artifact job storageId [{}] repositoryId [{}] storageCondition [{}] storageDay [{}] path [{}] error [{}]", storageId, repositoryId, storageCondition, storageDay, path, ExceptionUtils.getStackTrace(ex));
                }
            }
            long success = resultList.stream().filter(ONE::equals).count(), fail = resultList.stream().filter(ZERO::equals).count();
            log.info("Cleanup artifact job storageId [{}] repositoryId [{}] storageCondition [{}] storageDay [{}] success [{}] fail [{}]",
                    storageId, repositoryId, storageCondition, storageDay, success, fail);
        } catch (Exception e) {
            log.error("Cleanup artifact job storageId [{}] repositoryId [{}] storageCondition [{}] storageDay [{}] error [{}]", storageId, repositoryId, storageCondition, storageDay, ExceptionUtils.getStackTrace(e));
        }
    }

    @Override
    public void cleanupV2(String storageId, String repositoryId, String path, String storageDay, String storageCondition, Map<String, String> cleanupArtifactPathMap) throws Exception {
        try {
            RepositoryPath rootRepositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
            String blobs = "blobs", manifest = "manifest", tag = "tag";
            List<Integer> resultList = Lists.newArrayList();
            List<String> excludeList = Lists.newArrayList(blobs, manifest);
            RepositoryPathUtil.handlerDockerImagePaths(rootRepositoryPath,
                    (RepositoryPath filePath) ->
                    {
                        handlerFilePath(resultList, excludeList, tag, storageId, repositoryId, filePath, storageDay, storageCondition, cleanupArtifactPathMap);
                    },
                    (RepositoryPath dirPath) -> {
                        handlerDirPath(storageId, repositoryId, dirPath);
                    });
            long success = resultList.stream().filter(ONE::equals).count(), fail = resultList.stream().filter(ZERO::equals).count();
            log.info("Cleanup artifact job storageId [{}] repositoryId [{}] storageCondition [{}] storageDay [{}] success [{}] fail [{}]",
                    storageId, repositoryId, storageCondition, storageDay, success, fail);
        } catch (Exception e) {
            log.error("Cleanup artifact job storageId [{}] repositoryId [{}] storageCondition [{}] storageDay [{}] error [{}]", storageId, repositoryId, storageCondition, storageDay, ExceptionUtils.getStackTrace(e));
        }
    }

    private void handlerFilePath(List<Integer> resultList, List<String> excludeList, String tag, String storageId, String repositoryId, RepositoryPath repositoryPath, String storageDay, String storageCondition, Map<String, String> cleanupArtifactPathMap) {
        try {
            log.info("Cleanup artifact job storageId [{}] repositoryId [{}] storageCondition [{}] storageDay [{}] imagePath [{}]", storageId, repositoryId, storageCondition, storageDay, repositoryPath);
            if (tag.equalsIgnoreCase(storageCondition)) {
                //按照tag保留
                handlerTag(storageId, repositoryId, storageDay, storageCondition, repositoryPath, excludeList, resultList, cleanupArtifactPathMap);
            } else {
                //按照天数保留
                handlerDay(storageId, repositoryId, storageDay, storageCondition, repositoryPath, excludeList, resultList, cleanupArtifactPathMap);
            }
        } catch (Exception ex) {
            log.error("Cleanup artifact job storageId [{}] repositoryId [{}] storageCondition [{}] storageDay [{}] path [{}] error [{}]", storageId, repositoryId, storageCondition, storageDay, repositoryPath, ExceptionUtils.getStackTrace(ex));
        }
    }

    private void handlerDirPath(String storageId, String repositoryId, RepositoryPath repositoryPath) {
        try {
            if (Files.exists(repositoryPath) && !Files.isSameFile(repositoryPath.getRoot(), repositoryPath) && RepositoryFiles.isDirectoryEmpty(repositoryPath)) {
                Files.deleteIfExists(repositoryPath);
                log.info("Cleanup storageId [{}] repositoryId [{}] dir path [{}] do delete", storageId, repositoryId, repositoryPath.toString());
            }
        } catch (Exception ex) {
            log.error("Cleanup artifact job storageId [{}] repositoryId [{}] dir path [{}] error [{}]", storageId, repositoryId, repositoryPath, ExceptionUtils.getStackTrace(ex));
        }
    }

    private void handlerTag(String storageId, String repositoryId, String storageDay, String storageCondition, RepositoryPath imageRepositoryPath, List<String> excludeList, List<Integer> resultList, Map<String, String> cleanupArtifactPathMap) throws Exception {
        Long storageQuantity = Long.parseLong(getCleanupDay(RepositoryFiles.relativizePath(imageRepositoryPath), "", storageDay, cleanupArtifactPathMap));
        if (Long.valueOf(ZERO).equals(storageQuantity)) {
            log.info("Cleanup storageId [{}] repositoryId [{}] path [{}] storageQuantity is zero skip", storageId, repositoryId, imageRepositoryPath);
            return;
        }
        List<Path> tagRepositoryPathList = getTags(imageRepositoryPath, excludeList);
        log.info("Cleanup artifact job storageId [{}] repositoryId [{}] storageCondition [{}] storage quantity [{}] imagePath [{}] tag quantity [{}] tags [{}]", storageId, repositoryId, storageCondition, storageQuantity, imageRepositoryPath, tagRepositoryPathList.size(), tagRepositoryPathList.stream().map(p -> p.getFileName().toString()).collect(Collectors.joining(",")));
        if (CollectionUtils.isEmpty(tagRepositoryPathList) || tagRepositoryPathList.size() <= storageQuantity) {
            log.info("Cleanup artifact job storageId [{}] repositoryId [{}] storageCondition [{}] imagePath [{}] tag quantity [{}] less than or equal storage quantity [{}] skip", storageId, repositoryId, storageCondition, imageRepositoryPath, tagRepositoryPathList.size(), storageQuantity);
            return;
        }
        RepositoryPath repositoryPath, manifestRepositoryPath;
        Map<RepositoryPath, Long> sortMap = Maps.newHashMap();
        Artifact artifact = null;
        for (Path path : tagRepositoryPathList) {
            log.info("Cleanup artifact job storageId [{}] repositoryId [{}] storageCondition [{}] storage quantity [{}] imagePath [{}] tag [{}]", storageId, repositoryId, storageCondition, storageQuantity, imageRepositoryPath, path);
            repositoryPath = (RepositoryPath) path;
            manifestRepositoryPath = getManifestPath(repositoryPath);
            if (Objects.isNull(manifestRepositoryPath)) {
                log.warn("Cleanup storageId [{}] repositoryId [{}] path [{}] manifest not found", storageId, repositoryId, repositoryPath);
                continue;
            }
            artifact = manifestRepositoryPath.getArtifactEntry();
            if (Objects.isNull(artifact)) {
                log.warn("Cleanup storageId [{}] repositoryId [{}] path [{}] artifact not found", storageId, repositoryId, manifestRepositoryPath);
                continue;
            }
            sortMap.put(manifestRepositoryPath, artifact.getCreated().toInstant(ZoneOffset.of("+8")).toEpochMilli());
        }
        sortMap = sortMap.entrySet().stream()
                .sorted(Map.Entry.comparingByValue())
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue,
                        (oldValue, newValue) -> oldValue, LinkedHashMap::new));
        log.info("Cleanup artifact job storageId [{}] repositoryId [{}] storageCondition [{}] storage quantity [{}] imagePath [{}] tag quantity [{}] order tags [{}]", storageId, repositoryId, storageCondition, storageQuantity, imageRepositoryPath, tagRepositoryPathList.size(), JSONObject.toJSONString(sortMap));
        for (Map.Entry<RepositoryPath, Long> manifestEntry : sortMap.entrySet()) {
            Long currentTagSize = getTagSize(imageRepositoryPath, excludeList);
            log.info("Cleanup artifact job storageId [{}] repositoryId [{}] storageCondition [{}] storage quantity [{}] imagePath [{}] tag [{}] currentTagSize [{}]", storageId, repositoryId, storageCondition, storageQuantity, imageRepositoryPath, manifestEntry.getKey(), currentTagSize);
            if (currentTagSize > storageQuantity) {
                Integer result = cleanupArtifact(manifestEntry.getKey(), storageId, repositoryId, MINUS_ONE.toString(), null);
                if (Objects.nonNull(result)) {
                    resultList.add(result);
                }
            } else {
                log.info("Cleanup artifact job storageId [{}] repositoryId [{}] storageCondition [{}] storage quantity [{}] imagePath [{}] tag [{}] currentTagSize [{}] cleanup over", storageId, repositoryId, storageCondition, storageQuantity, imageRepositoryPath, manifestEntry.getKey(), currentTagSize);
                break;
            }
        }
    }

    private void handlerDay(String storageId, String repositoryId, String storageDay, String storageCondition, RepositoryPath imageRepositoryPath, List<String> excludeList, List<Integer> resultList, Map<String, String> cleanupArtifactPathMap) throws Exception {
        List<Path> tagRepositoryPathList = getTags(imageRepositoryPath, excludeList);
        log.info("Cleanup artifact job storageId [{}] repositoryId [{}] storageCondition [{}] storage quantity [{}] imagePath [{}] tags [{}]", storageId, repositoryId, storageCondition, storageDay, imageRepositoryPath, tagRepositoryPathList.stream().map(p -> p.getFileName().toString()).collect(Collectors.joining(",")));
        if (CollectionUtils.isEmpty(tagRepositoryPathList)) {
            return;
        }
        RepositoryPath tagRepositoryPath;
        for (Path tagPath : tagRepositoryPathList) {
            tagRepositoryPath = (RepositoryPath) tagPath;
            log.info("Cleanup artifact job storageId [{}] repositoryId [{}] storageCondition [{}] storageDay [{}] imagePath [{}] tagPath [{}]", storageId, repositoryId, storageCondition, storageDay, imageRepositoryPath, tagPath);
            try {
                Integer result = cleanupArtifact(getManifestPath(tagRepositoryPath), storageId, repositoryId, storageDay, cleanupArtifactPathMap);
                if (Objects.nonNull(result)) {
                    resultList.add(result);
                }
            } catch (Exception ex) {
                log.error("Cleanup artifact job storageId [{}] repositoryId [{}] storageCondition [{}] storageDay [{}] imagePath [{}] tagPath [{}] error [{}]", storageId, repositoryId, storageCondition, storageDay, imageRepositoryPath, tagPath, ExceptionUtils.getStackTrace(ex));
            }
        }
    }

    private Integer cleanupArtifact(RepositoryPath repositoryPath, String storageId, String repositoryId, String storageDay, Map<String, String> cleanupArtifactPathMap) throws Exception {
        if (Objects.isNull(repositoryPath) || !Files.exists(repositoryPath)) {
            log.warn("Cleanup storageId [{}] repositoryId [{}] path [{}] file not exists", storageId, repositoryId, repositoryPath);
            return null;
        }
        String artifactPath = RepositoryFiles.relativizePath(repositoryPath);
        if (Files.isDirectory(repositoryPath)) {
            log.warn("Cleanup storageId [{}] repositoryId [{}] path [{}] is directory skip", storageId, repositoryId, repositoryPath);
            return null;
        }
        String path = repositoryPath.toString();
        boolean checkDockerTag = repositoryPath.getFileName().toString().startsWith(PREFIX) && !path.contains("blobs/sha256") && !path.contains("manifest/sha256");
        if (!checkDockerTag) {
            log.info("Cleanup storageId [{}] repositoryId [{}] path [{}] not a docker tag file skip", storageId, repositoryId, path);
            return null;
        }
        if (RepositoryFiles.isTemp(repositoryPath)) {
            log.info("Cleanup storageId [{}] repositoryId [{}] path [{}] is temp file skip", storageId, repositoryId, path);
            return null;
        }
        if (RepositoryFiles.isChecksum(repositoryPath)) {
            log.info("Cleanup storageId [{}] repositoryId [{}] path [{}] is checksum file skip", storageId, repositoryId, path);
            return null;
        }
        if (!RepositoryFiles.isArtifact(repositoryPath)) {
            log.info("Cleanup storageId [{}] repositoryId [{}] path [{}] not is artifact file skip", storageId, repositoryId, path);
            return null;
        }
        log.info("Cleanup storageId [{}] repositoryId [{}] path [{}] is a docker tag file", storageId, repositoryId, path);
        Artifact artifact = repositoryPath.getArtifactEntry();
        if (null == artifact || null == artifact.getLastUsed()) {
            log.warn("Cleanup storageId [{}] repositoryId [{}] path [{}] artifact not found", storageId, repositoryId, path);
            return null;
        }
        RepositoryPath manifestRepositoryPath = repositoryPathResolver.resolve(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), "manifest" + File.separator + repositoryPath.getFileName().toString());
        if (!Files.exists(manifestRepositoryPath)) {
            log.warn("Cleanup storageId [{}] repositoryId [{}] path [{}] manifest file not exists", storageId, repositoryId, manifestRepositoryPath.toString());
            return null;
        }
        Artifact manifestArtifact = manifestRepositoryPath.getArtifactEntry();
        if (null == manifestArtifact || null == manifestArtifact.getLastUsed()) {
            log.warn("Cleanup storageId [{}] repositoryId [{}] path [{}] manifest artifact not found", storageId, repositoryId, manifestRepositoryPath.toString());
            return null;
        }
        long cleanupDay = Long.parseLong(getCleanupDay(artifactPath, artifact.getMetadata(), storageDay, cleanupArtifactPathMap));
        if (Long.valueOf(ZERO).equals(cleanupDay)) {
            log.info("Cleanup storageId [{}] repositoryId [{}] path [{}] cleanupDay is zero skip", storageId, repositoryId, path);
            return null;
        }
        //获取仓库下制品最近使用时间做比较
        LocalDateTime tagTime = artifact.getLastUsed();
        LocalDateTime manifestTime = manifestArtifact.getLastUsed();
        log.info("Cleanup docker storageId [{}] repositoryId [{}] storageDay [{}] path [{}] time [{}] manifest time [{}] current time [{}]", storageId, repositoryId, cleanupDay, artifact.getArtifactPath(), tagTime, manifestTime, LocalDateTime.now());
        boolean canDelete = (!LocalDateTime.now().minusDays(cleanupDay).isBefore(tagTime) && !LocalDateTime.now().minusDays(cleanupDay).isBefore(manifestTime)) || MINUS_ONE.equals(cleanupDay);
        if (canDelete) {
            try {
                RepositoryPath deleteRepositoryPath = repositoryPath.getParent();
                log.info("Cleanup docker tag storageId [{}] repositoryId [{}] path [{}] do delete", storageId, repositoryId, deleteRepositoryPath.toString());
                artifactManagementService.delete(deleteRepositoryPath, repositoryPath.getRepository().isAllowsForceDeletion());
                RepositoryPath dockerImageRepositoryPath = deleteRepositoryPath.getParent();
                if (Files.exists(dockerImageRepositoryPath) && !Files.isSameFile(repositoryPath.getRoot(), dockerImageRepositoryPath) && RepositoryFiles.isDirectoryEmpty(dockerImageRepositoryPath)) {
                    Files.deleteIfExists(dockerImageRepositoryPath);
                    log.info("Cleanup docker image storageId [{}] repositoryId [{}] path [{}] do delete", storageId, repositoryId, dockerImageRepositoryPath.toString());
                }
                return ONE;
            } catch (Exception e) {
                log.error("Cleanup storageId [{}] repositoryId [{}] path [{}] error [{}]", storageId, repositoryId, path, ExceptionUtils.getStackTrace(e));
                return ZERO;
            }
        }
        return null;
    }

    /**
     * 获取仓库下的tag列表
     *
     * @param repositoryPath 仓库路径
     * @param excludeList    要排除的列表
     * @return 目录列表
     * @throws IOException 异常
     */
    private List<Path> getTags(RepositoryPath repositoryPath, List<String> excludeList)
            throws IOException {
        List<Path> directoryList;
        try (Stream<Path> pathStream = Files.list(repositoryPath)) {
            directoryList = pathStream.filter(p -> !p.toString().startsWith(".") && !p.toString().contains("/.") && excludeList.stream().noneMatch(p.getFileName().toString()::equals) && RepositoryPathUtil.isDockerTag((RepositoryPath) p))
                    .filter(p -> {
                        try {
                            return !Files.isHidden(p) && Files.isDirectory(p);
                        } catch (IOException e) {
                            log.warn("Error accessing path [{}] error [{}]", p, ExceptionUtils.getStackTrace(e));
                            return false;
                        }
                    })
                    .sorted()
                    .collect(Collectors.toList());
        }
        return directoryList;
    }

    /**
     * 获取tag个数
     *
     * @param repositoryPath 制品路径
     * @param excludeList    要排除的列表
     * @return tag个数
     * @throws IOException 异常
     */
    private Long getTagSize(RepositoryPath repositoryPath, List<String> excludeList)
            throws IOException {
        Long size;
        try (Stream<Path> pathStream = Files.list(repositoryPath)) {
            size = pathStream.filter(p -> !p.toString().startsWith(".") && !p.toString().contains("/.") && excludeList.stream().noneMatch(p.getFileName().toString()::equals) && RepositoryPathUtil.isDockerTag((RepositoryPath) p))
                    .filter(p -> {
                        try {
                            return !Files.isHidden(p) && Files.isDirectory(p);
                        } catch (IOException e) {
                            log.warn("Error accessing path [{}] error [{}]", p, ExceptionUtils.getStackTrace(e));
                            return false;
                        }
                    })
                    .count();
        }
        return size;
    }

    /**
     * 获取仓库下的tag路径
     *
     * @param repositoryPath 仓库路径
     * @return tag路径
     * @throws IOException 异常
     */
    private RepositoryPath getManifestPath(RepositoryPath repositoryPath)
            throws IOException {
        RepositoryPath path = null;
        List<String> excludeList = Lists.newArrayList("temp", ".temp");
        try (Stream<Path> pathStream = Files.list(repositoryPath)) {
            List<Path> pathList = pathStream.filter(p -> {
                try {
                    RepositoryPath itemRepositoryPath = (RepositoryPath) p;
                    return p.getFileName().toString().startsWith(PREFIX) && !p.toString().startsWith(".") && !p.toString().contains("/.") && excludeList.stream().noneMatch(p.getFileName().toString()::equals) &&
                            !Files.isHidden(p) && !Files.isDirectory(p) && !RepositoryFiles.isChecksum(itemRepositoryPath) && !RepositoryFiles.isArtifactMetadata(itemRepositoryPath);
                } catch (IOException e) {
                    log.warn("Error accessing path [{}] error [{}]", p, ExceptionUtils.getStackTrace(e));
                    return false;
                }
            })
                    .sorted()
                    .collect(Collectors.toList());
            if (CollectionUtils.isNotEmpty(pathList)) {
                path = (RepositoryPath) pathList.get(0);
                path.setArtifact(repositoryPathResolver.resolve(path.getStorageId(), path.getRepositoryId(), RepositoryFiles.relativizePath(path)).getArtifactEntry());

            }
            log.info("Tag [{}] manifestRepositoryPath [{}]", repositoryPath, path);
        }
        return path;
    }

    private String getCleanupDay(String artifactPath, String metadata, String cleanupDay, Map<String, String> cleanupArtifactPathMap) {
        if (MINUS_ONE.toString().equals(cleanupDay)) {
            return cleanupDay;
        }
        if (StringUtils.isNotBlank(metadata) && JSONUtil.isJson(metadata)) {
            //获取元数据级别生命周期，优先级最高
            JSONObject metadataJson = JSONObject.parseObject(metadata);
            if (metadataJson.containsKey(GlobalConstants.ARTIFACT_LIFE_CYCLE_KEY)) {
                String artifactLifeCycleData = metadataJson.getString(GlobalConstants.ARTIFACT_LIFE_CYCLE_KEY);
                if (StringUtils.isNotBlank(artifactLifeCycleData) && JSONUtil.isJson(artifactLifeCycleData)) {
                    JSONObject artifactLifeCycleJson = JSONObject.parseObject(artifactLifeCycleData);
                    String artifactLifeCycle = artifactLifeCycleJson.getString("value");
                    if (GlobalConstants.ARTIFACT_RETENTION_FOREVER_KEY.equalsIgnoreCase(artifactLifeCycle)) {
                        return "0";
                    }
                    if (StringUtils.isNotBlank(artifactLifeCycle) && StringUtils.isNumeric(artifactLifeCycle)) {
                        //制品元数据级别生命周期
                        return artifactLifeCycle;
                    }
                }
            }
        }
        if (MapUtils.isEmpty(cleanupArtifactPathMap)) {
            return cleanupDay;
        }
        String cleanupArtifactPath, cleanupArtifactPathValue, cleanupArtifactPathPrefix;
        for (Map.Entry<String, String> entry : cleanupArtifactPathMap.entrySet()) {
            cleanupArtifactPath = entry.getKey();
            cleanupArtifactPathValue = entry.getValue();
            if (StringUtils.isBlank(cleanupArtifactPath) || StringUtils.isBlank(cleanupArtifactPathValue)) {
                continue;
            }
            //获取目录、制品级别生命周期，优先级第二
            cleanupArtifactPathPrefix = cleanupArtifactPath + GlobalConstants.SEPARATOR;
            if (artifactPath.equals(cleanupArtifactPath) || artifactPath.startsWith(cleanupArtifactPathPrefix) || artifactPath.matches(cleanupArtifactPath)) {
                return entry.getValue();
            }
        }
        //仓库级别生命周期，优先级最低
        return cleanupDay;
    }
}
