package com.veadan.folib.task;


import cn.hutool.core.date.DateUtil;
import cn.hutool.json.JSONUtil;
import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.components.artifact.ArtifactComponent;
import com.veadan.folib.domain.ArtifactEventRecord;
import com.veadan.folib.event.artifact.ArtifactEventTypeEnum;
import com.veadan.folib.eventlistener.webhook.ArtifactEventWebhookListener;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.repository.ArtifactDownloadingEventHandler;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.compress.utils.Lists;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.LineIterator;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author leipenghui
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
    private ArtifactDownloadingEventHandler artifactDownloadingEventHandler;

    @Autowired
    @Lazy
    private ArtifactEventWebhookListener artifactEventWebhookListener;

    @Autowired
    @Lazy
    private RepositoryPathResolver repositoryPathResolver;

    @Scheduled(cron = "0 0 1 * * ?")
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
                    int batch = 1000;
                    for (Path eventPath : pathList) {
                        String currentLine = "";
                        ArtifactEventRecord artifactEventRecord;
                        List<ArtifactEventRecord> artifactEventRecordList = Lists.newArrayList();
                        RepositoryPath repositoryPath;
                        long lines = 0, startTime = System.currentTimeMillis();
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
                                    //制品下载中事件
                                    repositoryPath = repositoryPathResolver.resolve(artifactEventRecord.getStorageId(), artifactEventRecord.getRepositoryId(), artifactEventRecord.getArtifactPath());
                                    if (!Files.exists(repositoryPath)) {
                                        continue;
                                    }
                                    if (ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_DOWNLOADING.getType() == artifactEventRecord.getEventType()) {
                                        artifactEventRecordList.add(artifactEventRecord);
                                    } else if (ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_DOWNLOADED.getType() == artifactEventRecord.getEventType()) {
                                        artifactEventRecordList.add(artifactEventRecord);
                                    }
                                    if (artifactEventRecordList.size() == batch) {
                                        clear(artifactEventRecordList, lines);
                                    }
                                } catch (Exception ex) {
                                    log.warn(ExceptionUtils.getStackTrace(ex));
                                }
                            }
                            if (CollectionUtils.isNotEmpty(artifactEventRecordList)) {
                                clear(artifactEventRecordList, lines);
                            }
                        }
                        log.info("Handle eventPath [{}] lines [{}] finished take time [{}] ms", eventPath.toString(), lines, System.currentTimeMillis() - startTime);
                        deletePathList.add(eventPath);
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

    private void clear(List<ArtifactEventRecord> artifactEventRecordList, Long lines) throws IOException {
        if (CollectionUtils.isEmpty(artifactEventRecordList)) {
            return;
        }
        RepositoryPath repositoryPath;
        Map<String, List<ArtifactEventRecord>> artifactEventRecordGroupMap = artifactEventRecordList.stream().collect(Collectors.groupingBy(item -> item.getPath() + "_" + item.getEventType()));
        for (Map.Entry<String, List<ArtifactEventRecord>> entry : artifactEventRecordGroupMap.entrySet()) {
            ArtifactEventRecord artifactEventRecordHandle = entry.getValue().get(0);
            long size = entry.getValue().size();
            repositoryPath = repositoryPathResolver.resolve(artifactEventRecordHandle.getStorageId(), artifactEventRecordHandle.getRepositoryId(), artifactEventRecordHandle.getArtifactPath());
            if (!Files.exists(repositoryPath)) {
                continue;
            }
            if (ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_DOWNLOADING.getType() == artifactEventRecordHandle.getEventType()) {
                artifactDownloadingEventHandler.handleEventRecord(repositoryPath, size, true);
                log.info("Handle current line [{}] repositoryPath [{}] EVENT_ARTIFACT_FILE_DOWNLOADING finished artifactEventRecordList size [{}]", lines, repositoryPath.toString(), artifactEventRecordList.size());
            } else if (ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_DOWNLOADED.getType() == artifactEventRecordHandle.getEventType()) {
                artifactEventWebhookListener.handleEventRecord(ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_DOWNLOADED, repositoryPath);
                log.info("Handle current line [{}] repositoryPath [{}] EVENT_ARTIFACT_FILE_DOWNLOADED finished artifactEventRecordList size [{}]", lines, repositoryPath.toString(), artifactEventRecordList.size());
            }
        }
        //清理
        artifactEventRecordList.clear();
        log.info("Clear artifactEventRecordList size [{}]", artifactEventRecordList.size());
    }
}
