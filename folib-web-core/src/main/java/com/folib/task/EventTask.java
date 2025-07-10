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
package com.folib.task;


import cn.hutool.core.date.DateUtil;
import cn.hutool.json.JSONUtil;
import com.alibaba.fastjson.JSONObject;
import com.google.common.collect.Lists;
import com.folib.components.artifact.ArtifactComponent;
import com.folib.domain.ArtifactEventRecord;
import com.folib.event.artifact.ArtifactEventTypeEnum;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.providers.repository.ArtifactDownloadedEventHandler;
import com.folib.utils.MapUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.LineIterator;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.concurrent.FutureTask;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author veadan
 * 事件处理task
 */
@Slf4j
@Component
@EnableScheduling
public class EventTask {

    @Autowired
    @Lazy
    private ArtifactComponent artifactComponent;

    @Autowired
    @Lazy
    private ArtifactDownloadedEventHandler artifactDownloadedEventHandler;

    @Autowired
    @Lazy
    private RepositoryPathResolver repositoryPathResolver;

    @Autowired
    @Lazy
    private ThreadPoolTaskExecutor asyncEventLogThreadPoolTaskExecutor;

    @Scheduled(cron = "0 0 0/1 * * ? ")
    public void run() {
        log.info("EventTask starting time [{}]", DateUtil.now());
        handle("");
        log.info("EventTask end time [{}]", DateUtil.now());
    }

    public void handle(String filename) {
        Path eventParentPath = Path.of(artifactComponent.getEventParentPath());
        if (Files.exists(eventParentPath)) {
            List<Path> deletePathList = Lists.newArrayList();
            try (Stream<Path> pathStream = Files.list(eventParentPath)) {
                List<Path> pathList = pathStream.sorted().collect(Collectors.toList());
                if (StringUtils.isNotBlank(filename)) {
                    pathList = pathList.stream().filter(p -> p.getFileName().toString().equals(filename)).collect(Collectors.toList());
                }
                if (CollectionUtils.isNotEmpty(pathList)) {
                    int batch = 3000;
                    for (Path eventPath : pathList) {
                        String currentLine = "";
                        ArtifactEventRecord artifactEventRecord;
                        List<ArtifactEventRecord> artifactEventRecordList = Lists.newArrayList();
                        RepositoryPath repositoryPath;
                        long lines = 0, startTime = System.currentTimeMillis();
                        try {
                            try (LineIterator lineIterator = FileUtils.lineIterator(eventPath.toFile(), "UTF-8")) {
                                while (lineIterator.hasNext()) {
                                    try {
                                        lines++;
                                        currentLine = lineIterator.nextLine();
                                        if (StringUtils.isBlank(currentLine) || !JSONUtil.isJson(currentLine)) {
                                            continue;
                                        }
                                        artifactEventRecord = JSONObject.parseObject(currentLine, ArtifactEventRecord.class);
                                        artifactEventRecord.setPath(String.format("%s-%s-%s", artifactEventRecord.getStorageId(), artifactEventRecord.getRepositoryId(), artifactEventRecord.getArtifactPath()));
                                        repositoryPath = repositoryPathResolver.resolve(artifactEventRecord.getStorageId(), artifactEventRecord.getRepositoryId(), artifactEventRecord.getArtifactPath());
                                        if (!Files.exists(repositoryPath)) {
                                            continue;
                                        }
                                        if (ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_DOWNLOADED.getType() == artifactEventRecord.getEventType()) {
                                            artifactEventRecordList.add(artifactEventRecord);
                                        }
                                        if (artifactEventRecordList.size() == batch) {
                                            clear(eventPath, artifactEventRecordList, lines);
                                        }
                                    } catch (Exception ex) {
                                        log.warn(ExceptionUtils.getStackTrace(ex));
                                    }
                                }
                                if (CollectionUtils.isNotEmpty(artifactEventRecordList)) {
                                    clear(eventPath, artifactEventRecordList, lines);
                                }
                            }
                        } catch (Exception ex) {
                            log.warn("Handle eventPath [{}] lines [{}] error [{}] ms", eventPath.toString(), lines, ExceptionUtils.getStackTrace(ex));
                        }
                        log.info("Handle eventPath [{}] lines [{}] finished take time [{}] ms", eventPath.toString(), lines, System.currentTimeMillis() - startTime);
                        deletePathList.add(eventPath);
                        Files.deleteIfExists(eventPath);
                    }
                }
            } catch (Exception ex) {
                log.warn(ExceptionUtils.getStackTrace(ex));
            }
            if (CollectionUtils.isNotEmpty(deletePathList)) {
                for (Path eventPath : deletePathList) {
                    try {
                        Files.deleteIfExists(eventPath);
                    } catch (Exception ex) {
                        log.error("Delete eventPath [{}] error [{}]", eventPath.toString(), ExceptionUtils.getStackTrace(ex));
                    }
                }
            }
        }
    }

    private void clear(Path eventPath, List<ArtifactEventRecord> artifactEventRecordList, Long lines) throws IOException {
        if (CollectionUtils.isEmpty(artifactEventRecordList)) {
            return;
        }
        Map<String, List<ArtifactEventRecord>> artifactEventRecordGroupMap = artifactEventRecordList.stream().collect(Collectors.groupingBy(item -> item.getPath() + "_" + item.getEventType()));
        List<Map<String, List<ArtifactEventRecord>>> list = MapUtil.splitByChunkSize(artifactEventRecordGroupMap, 100);
        if (CollectionUtils.isEmpty(list)) {
            return;
        }
        List<FutureTask<String>> futureTasks = Lists.newArrayList();
        FutureTask<String> futureTask = null;
        for (Map<String, List<ArtifactEventRecord>> map : list) {
            futureTask = new FutureTask<String>(() -> {
                for (Map.Entry<String, List<ArtifactEventRecord>> entry : map.entrySet()) {
                    ArtifactEventRecord artifactEventRecordHandle = entry.getValue().get(0);
                    long size = entry.getValue().size();
                    RepositoryPath repositoryPath = repositoryPathResolver.resolve(artifactEventRecordHandle.getStorageId(), artifactEventRecordHandle.getRepositoryId(), artifactEventRecordHandle.getArtifactPath());
                    if (!Files.exists(repositoryPath)) {
                        continue;
                    }
                    if (ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_DOWNLOADED.getType() == artifactEventRecordHandle.getEventType()) {
                        artifactDownloadedEventHandler.handleEventRecord(repositoryPath, size, true);
                        log.debug("Handle eventPath [{}] current batch lines [{}] repositoryPath [{}] EVENT_ARTIFACT_FILE_DOWNLOADED finished artifactEventRecordList size [{}]", eventPath.toString(), lines, repositoryPath.toString(), artifactEventRecordList.size());
                    }
                }
                return "success";
            });
            futureTasks.add(futureTask);
            asyncEventLogThreadPoolTaskExecutor.submit(futureTask);
        }
        futureTasks.forEach(action -> {
            try {
                action.get();
            } catch (Exception e) {
                log.error(e.getMessage(), e);
            }
        });
        //清理
        artifactEventRecordList.clear();
        log.debug("Clear artifactEventRecordList size [{}]", artifactEventRecordList.size());
    }
}
