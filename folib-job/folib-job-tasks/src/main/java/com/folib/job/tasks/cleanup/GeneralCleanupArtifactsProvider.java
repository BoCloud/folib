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
import com.folib.configuration.ConfigurationManager;
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
import java.nio.file.Files;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * @author veadan
 **/
@Slf4j
@Component
public class GeneralCleanupArtifactsProvider implements CleanupArtifactsProvider {

    @Inject
    private CleanupArtifactsProviderRegistry cleanupArtifactsProviderRegistry;

    @Inject
    private ArtifactManagementService artifactManagementService;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private ConfigurationManager configurationManager;

    private static final Integer ONE = 1;

    private static final Integer ZERO = 0;

    @PostConstruct
    @Override
    public void register() {
        cleanupArtifactsProviderRegistry.addProvider("GENERAL", this);
        log.info("Registered cleanup repository cron job '[{}]' with alias '[{}]'.",
                getClass().getCanonicalName(), "GENERAL");
    }

    @Override
    public void cleanup(String storageId, String repositoryId, String path, String storageDay, String storageCondition, Map<String, String> cleanupArtifactPathMap) {
        try {
            RepositoryPath rootRepositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
            List<RepositoryPath> repositoryPaths = RepositoryPathUtil.getPaths(rootRepositoryPath.getRepository().getLayout(), rootRepositoryPath);
            if (CollectionUtils.isEmpty(repositoryPaths)) {
                log.info("Repository storageId [{}] repositoryId [{}] not found artifacts", storageId, repositoryId);
                return;
            }
            log.info("Start cleanup artifact job [ storageId [{}] repositoryId [{}] storageCondition [{}] storageDay [{}]  artifactPaths [{}]", storageId, repositoryId, storageCondition, storageDay, repositoryPaths.size());
            List<Integer> resultList = Lists.newArrayList();
            for (RepositoryPath repositoryPath : repositoryPaths) {
                try {
                    Integer result = cleanupArtifact(storageId, repositoryId, repositoryPath, storageDay, cleanupArtifactPathMap);
                    if (Objects.nonNull(result)) {
                        resultList.add(result);
                    }
                } catch (Exception ex) {
                    log.error("Cleanup artifact job storageId [{}] repositoryId [{}] storageCondition [{}] storageDay [{}]  path [{}] error [{}]", storageId, repositoryId, storageCondition, storageDay, path, ExceptionUtils.getStackTrace(ex));
                }
            }
            long success = resultList.stream().filter(ONE::equals).count(), fail = resultList.stream().filter(ZERO::equals).count();
            log.info("Cleanup artifact job storageId [{}] repositoryId [{}] storageCondition [{}] storageDay [{}]  success [{}] fail [{}]",
                    storageId, repositoryId, storageCondition, storageDay, success, fail);
        } catch (Exception e) {
            log.error("Cleanup artifact job storageId [{}] repositoryId [{}] storageCondition [{}] storageDay [{}]  error [{}]", storageId, repositoryId, storageCondition, storageDay, ExceptionUtils.getStackTrace(e));
        }
    }

    @Override
    public void cleanupV2(String storageId, String repositoryId, String path, String storageDay, String storageCondition, Map<String, String> cleanupArtifactPathMap) throws Exception {
        try {
            RepositoryPath rootRepositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
            List<Integer> resultList = Lists.newArrayList();
            RepositoryPathUtil.handlerPaths(rootRepositoryPath.getRepository().getLayout(), rootRepositoryPath,
                    (RepositoryPath filePath) ->
                    {
                        handlerFilePath(resultList, storageId, repositoryId, filePath, storageDay, storageCondition, cleanupArtifactPathMap);
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

    private void handlerFilePath(List<Integer> resultList, String storageId, String repositoryId, RepositoryPath repositoryPath, String storageDay, String storageCondition, Map<String, String> cleanupArtifactPathMap) {
        try {
            Integer result = cleanupArtifact(storageId, repositoryId, repositoryPath, storageDay, cleanupArtifactPathMap);
            if (Objects.nonNull(result)) {
                resultList.add(result);
            }
        } catch (Exception ex) {
            log.error("Cleanup artifact job storageId [{}] repositoryId [{}] storageCondition [{}] storageDay [{}]  path [{}] error [{}]", storageId, repositoryId, storageCondition, storageDay, repositoryPath, ExceptionUtils.getStackTrace(ex));
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

    private Integer cleanupArtifact(String storageId, String repositoryId, RepositoryPath repositoryPath, String storageDay, Map<String, String> cleanupArtifactPathMap) throws Exception {
        String path = RepositoryFiles.relativizePath(repositoryPath);
        if (!Files.exists(repositoryPath)) {
            log.warn("Cleanup storageId [{}] repositoryId [{}] path [{}] file not exists", storageId, repositoryId, repositoryPath);
            return null;
        }
        if (Files.isDirectory(repositoryPath)) {
            log.warn("Cleanup storageId [{}] repositoryId [{}] path [{}] is directory skip", storageId, repositoryId, repositoryPath);
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
        Artifact artifact = repositoryPath.getArtifactEntry();
        if (null == artifact || null == artifact.getLastUsed()) {
            log.warn("Cleanup storageId [{}] repositoryId [{}] path [{}] artifact not found", storageId, repositoryId, path);
            return null;
        }
        long cleanupDay = Long.parseLong(getCleanupDay(path, artifact.getMetadata(), storageDay, cleanupArtifactPathMap));
        if (Long.valueOf(ZERO).equals(cleanupDay)) {
            log.info("Cleanup storageId [{}] repositoryId [{}] path [{}] cleanupDay is zero skip", storageId, repositoryId, path);
            return null;
        }
        //获取仓库下制品最近使用时间做比较
        LocalDateTime lastUsedTime = artifact.getLastUsed();
        log.info("Cleanup storageId [{}] repositoryId [{}] storageDay [{}] path [{}] lastUsedTime [{}] current time [{}]", storageId, repositoryId, cleanupDay, artifact.getArtifactPath(), lastUsedTime, LocalDateTime.now());
        if (!LocalDateTime.now().minusDays(cleanupDay).isBefore(lastUsedTime)) {
            try {
                log.info("Cleanup storageId [{}] repositoryId [{}] path [{}] do delete", storageId, repositoryId, repositoryPath.toString());
                artifactManagementService.delete(repositoryPath, repositoryPath.getRepository().isAllowsForceDeletion());
                RepositoryPath parentRepositoryPath = null;
                parentRepositoryPath = repositoryPath.getParent();
                if (Files.exists(parentRepositoryPath) && !Files.isSameFile(repositoryPath.getRoot(), parentRepositoryPath) && RepositoryFiles.isDirectoryEmpty(parentRepositoryPath)) {
                    Files.deleteIfExists(parentRepositoryPath);
                    log.info("Cleanup storageId [{}] repositoryId [{}] parent path [{}] do delete", storageId, repositoryId, parentRepositoryPath.toString());
                }
                return ONE;
            } catch (Exception e) {
                log.error("Cleanup storageId [{}] repositoryId [{}] path [{}] error [{}]", storageId, repositoryId, path, ExceptionUtils.getStackTrace(e));
                return ZERO;
            }
        }
        return null;
    }

    private String getCleanupDay(String artifactPath, String metadata, String cleanupDay, Map<String, String> cleanupArtifactPathMap) {
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
