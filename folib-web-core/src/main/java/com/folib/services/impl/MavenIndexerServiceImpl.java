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
package com.folib.services.impl;

import cn.hutool.core.io.FileUtil;
import com.alibaba.fastjson.JSONObject;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.folib.configuration.ConfigurationManager;
import com.folib.entity.Dict;
import com.folib.enums.DictTypeEnum;
import com.folib.enums.MavenIndexerBinTypeEnum;
import com.folib.providers.io.RepositoryPath;
import com.folib.services.ArtifactResolutionService;
import com.folib.services.DictService;
import com.folib.services.MavenIndexerService;
import com.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.exec.CommandLine;
import org.apache.commons.exec.DefaultExecutor;
import org.apache.commons.exec.ExecuteWatchdog;
import org.apache.commons.exec.PumpStreamHandler;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.LineIterator;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.SystemUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Service;

import javax.inject.Inject;
import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.attribute.PosixFilePermission;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.FutureTask;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.atomic.AtomicLong;
import java.util.stream.Collectors;

/**
 * @author veadan
 */
@Service
@Slf4j
public class MavenIndexerServiceImpl implements MavenIndexerService {

    @Value("${folib.temp}")
    private String tempPath;

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private DictService dictService;

    @Inject
    private ThreadPoolTaskExecutor asyncFetchRemotePackageThreadPoolTaskExecutor;

    @Inject
    private ArtifactResolutionService artifactResolutionService;

    @Override
    public String storeMavenIndexer(String format, String indexId, String chainId, String url) {
        String targetPath = "", binPath = "";
        try {
            url = StringUtils.removeEnd(url, "/");
            targetPath = tempPath + File.separator + UUID.randomUUID() + File.separator + indexId + "_index.dump";
            File file = new File(targetPath);
            long startTime = System.currentTimeMillis();
            log.info("存储MavenIndexer format [{}] indexId [{}] chainId [{}] url [{}] targetPath [{}] 开始", format, indexId, chainId, url, targetPath);

            binPath = getBinPath();

            // 构建命令行参数
            CommandLine commandLine = CommandLine.parse(binPath)
                    .addArgument("--format").addArgument(format)
                    .addArgument("--indexId").addArgument(indexId)
                    .addArgument("--chainId").addArgument(chainId)
                    .addArgument("--url").addArgument(url);

            // 设置执行器和输出流处理器
            DefaultExecutor executor = new DefaultExecutor();
            executor.setExitValue(0); // 假设正常退出码为 0

            // 设置超时（60秒）
            ExecuteWatchdog watchdog = new ExecuteWatchdog(60_000);
            executor.setWatchdog(watchdog);

            // 将标准输出写入文件
            FileOutputStream outputStream = new FileOutputStream(file);
            PumpStreamHandler streamHandler = new PumpStreamHandler(outputStream);
            executor.setStreamHandler(streamHandler);

            // 执行命令
            int exitCode = executor.execute(commandLine);
            log.info("命令执行结束，退出码: {}", exitCode);

            log.info("存储MavenIndexer format [{}] indexId [{}] chainId [{}] url [{}] 结束 targetPath [{}] 耗时约为 [{}] 秒",
                    format, indexId, chainId, url, targetPath, (System.currentTimeMillis() - startTime) / 1000);

            return targetPath;

        } catch (Exception e) {
            log.error("存储MavenIndexer format [{}] indexId [{}] chainId [{}] url [{}] 错误 [{}]", format, indexId, chainId, url, ExceptionUtils.getStackTrace(e));
        } finally {
            if (StringUtils.isNotBlank(binPath)) {
                FileUtil.del(binPath);
            }
        }
        return targetPath;
    }


    @Async
    @Override
    public void handlerMavenIndexerAndDownLoad(String username, Repository repository, String mavenIndexerPath, Integer batch, Integer poolSize) {
        if (Objects.isNull(batch)) {
            batch = 500;
        }
        ThreadPoolTaskExecutor threadPoolTaskExecutor = null;
        if (Objects.nonNull(poolSize)) {
            threadPoolTaskExecutor = buildThreadPoolTaskExecutor(poolSize, poolSize);
        }
        File file = null;
        String dictKey = String.format("%s:%s", repository.getStorageIdAndRepositoryId(), System.currentTimeMillis()), storageId = repository.getStorage().getId(), repositoryId = repository.getId();
        JSONObject dictValueJson = new JSONObject();
        long lines = 0, artifactsCount = 0, startTime = System.currentTimeMillis();
        AtomicLong al = new AtomicLong(0), successAl = new AtomicLong(0), failAl = new AtomicLong(0);
        BigDecimal oneHundred = BigDecimal.valueOf(100);
        try {
            file = new File(mavenIndexerPath);
            dictValueJson.put("mavenIndexerFileName", file.getName());
            if (FileUtil.isDirectory(file) || !FileUtil.exist(file)) {
                log.warn("MavenIndexer storageId [{}] repositoryId [{}] mavenIndexerPath [{}] 文件不存在", storageId, repositoryId, mavenIndexerPath);
                return;
            }
            List<String> fileExtensionList = Lists.newArrayList("pom", "jar", "war", "ear", "zip"), artifactPathList = Lists.newArrayList();
            try (LineIterator lineIterator = FileUtils.lineIterator(file, "UTF-8")) {
                String fileExtension, groupId, artifactId, version, artifactPath, pom = "pom", currentLine = "";
                JSONObject itemData;
                while (lineIterator.hasNext()) {
                    try {
                        lines++;
                        currentLine = lineIterator.nextLine();
                        if (currentLine.startsWith("[") || currentLine.startsWith("]")) {
                            continue;
                        }
                        currentLine = StringUtils.chomp(currentLine, ",");
                        itemData = JSONObject.parseObject(currentLine);
                        fileExtension = itemData.getString("fileExtension");
                        groupId = itemData.getString("groupId").replaceAll("\\.", "/");
                        artifactId = itemData.getString("artifactId");
                        version = itemData.getString("version");
                        artifactPath = String.format("%s/%s/%s/%s-%s.%s", groupId, artifactId, version, artifactId, version, fileExtension);
                        if (fileExtensionList.contains(fileExtension)) {
                            artifactPathList.add(artifactPath);
                            if (!fileExtension.equals(pom)) {
                                artifactPath = String.format("%s/%s/%s/%s-%s.%s", groupId, artifactId, version, artifactId, version, pom);
                                artifactPathList.add(artifactPath);
                            }
                        }
                    } catch (Exception ex) {
                        log.error("同步MavenIndexer storageId [{}] repositoryId [{}] mavenIndexerPath [{}] currentLine [{}] 错误 [{}]", storageId, repositoryId, mavenIndexerPath, currentLine, ExceptionUtils.getStackTrace(ex));
                    }
                }
            }
            if (CollectionUtils.isEmpty(artifactPathList)) {
                log.info("同步MavenIndexer storageId [{}] repositoryId [{}] mavenIndexerPath [{}] 未找到匹配的制品数据", storageId, repositoryId, mavenIndexerPath);
                return;
            }
            artifactPathList = artifactPathList.stream().distinct().collect(Collectors.toList());
            artifactsCount = artifactPathList.size();
            long point = artifactsCount / 100;
            BigDecimal bCount = new BigDecimal(artifactsCount);
            dictValueJson.put("storageId", repository.getStorage().getId());
            dictValueJson.put("repositoryId", repository.getId());
            dictValueJson.put("lines", lines);
            dictValueJson.put("artifactsCount", artifactsCount);
            dictValueJson.put("progress", 0);
            dictValueJson.put("process", 0);
            dictValueJson.put("success", 0);
            dictValueJson.put("fail", 0);
            dictValueJson.put("operator", username);
            handlerDownLoadStatus(dictKey, dictValueJson.toJSONString(), "迁移中");
            log.info("开始同步MavenIndexer storageId [{}] repositoryId [{}] mavenIndexerPath [{}] batch [{}] 找到 [{}] 条匹配的制品数据", storageId, repositoryId, mavenIndexerPath, batch, artifactsCount);
            List<List<String>> artifactPathLists = Lists.partition(artifactPathList, batch);
            FutureTask<String> futureTask = null;
            List<FutureTask<String>> futureTaskList = Lists.newArrayList();
            for (List<String> artifactPaths : artifactPathLists) {
                futureTask = new FutureTask<String>(() -> {
                    for (String itemArtifactPath : artifactPaths) {
                        long current = al.getAndIncrement();
                        try {
                            RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, itemArtifactPath);
                            boolean flag = Files.exists(repositoryPath);
                            if (flag) {
                                log.info("同步MavenIndexer storageId [{}] repositoryId [{}] artifactPath [{}] mavenIndexerPath [{}] 第 [{}] 个有效行, success [{}]", storageId, repositoryId, itemArtifactPath, mavenIndexerPath, current, successAl.getAndIncrement());
                            } else {
                                log.warn("同步MavenIndexer storageId [{}] repositoryId [{}] artifactPath [{}] mavenIndexerPath [{}] 第 [{}] 个有效行, fail [{}] success [{}]", storageId, repositoryId, itemArtifactPath, mavenIndexerPath, current, failAl.getAndIncrement(), successAl.get());
                            }
                            if (current % point == 0) {
                                dictValueJson.put("progress", BigDecimal.valueOf(current).divide(bCount, 2, RoundingMode.HALF_UP).multiply(oneHundred));
                                dictValueJson.put("process", al.get());
                                dictValueJson.put("success", successAl.get());
                                dictValueJson.put("fail", failAl.get());
                                handlerDownLoadStatus(dictKey, dictValueJson.toJSONString(), "迁移中");
                            }
                        } catch (Exception ex) {
                            log.warn("同步MavenIndexer storageId [{}] repositoryId [{}] artifactPath [{}] mavenIndexerPath [{}] 第 [{}] 个有效行, fail [{}]", storageId, repositoryId, itemArtifactPath, mavenIndexerPath, current, failAl.get());
                        }
                    }
                    return "success";
                });
                futureTaskList.add(futureTask);
                if (Objects.nonNull(threadPoolTaskExecutor)) {
                    threadPoolTaskExecutor.submit(futureTask);
                } else {
                    asyncFetchRemotePackageThreadPoolTaskExecutor.submit(futureTask);
                }
            }
            for (FutureTask<String> task : futureTaskList) {
                task.get();
            }
            dictValueJson.put("progress", BigDecimal.valueOf(al.get()).divide(bCount, 2, RoundingMode.HALF_UP).multiply(oneHundred));
            dictValueJson.put("process", al.get());
            dictValueJson.put("success", successAl.get());
            dictValueJson.put("fail", failAl.get());
            dictValueJson.put("takeTime", System.currentTimeMillis() - startTime);
            handlerDownLoadStatus(dictKey, dictValueJson.toJSONString(), "迁移完成");
        } catch (Exception ex) {
            log.error("同步MavenIndexer storageId [{}] repositoryId [{}] mavenIndexerPath [{}] 错误 [{}]", storageId, repositoryId, mavenIndexerPath, ExceptionUtils.getStackTrace(ex));
            dictValueJson.put("progress", BigDecimal.valueOf(al.get()).divide(BigDecimal.valueOf(artifactsCount), 2, RoundingMode.HALF_UP).multiply(oneHundred));
            dictValueJson.put("process", al.get());
            dictValueJson.put("success", successAl.get());
            dictValueJson.put("fail", failAl.get());
            dictValueJson.put("takeTime", System.currentTimeMillis() - startTime);
            handlerDownLoadStatus(dictKey, dictValueJson.toJSONString(), "迁移错误");
        } finally {
            log.info("同步MavenIndexer storageId [{}] repositoryId [{}] mavenIndexerPath [{}] 结束 lines [{}] artifactsCount [{}] process [{}] fail [{}] success [{}] take time [{}]", storageId, repositoryId, mavenIndexerPath, lines, artifactsCount, al.get(), failAl.get(), successAl.get(), System.currentTimeMillis() - startTime);
            if (Objects.nonNull(file)) {
                FileUtil.del(file.getParent());
            }
            if (Objects.nonNull(threadPoolTaskExecutor)) {
                threadPoolTaskExecutor.shutdown();
                threadPoolTaskExecutor.destroy();
            }
        }
    }

    /**
     * 获取解析MavenIndexer的脚本路径
     *
     * @return 脚本路径
     */
    private String getBinPath() {
        try {
            String path = "";
            if (SystemUtils.IS_OS_LINUX) {
                path = MavenIndexerBinTypeEnum.UNIX.getPath();
            } else if (SystemUtils.IS_OS_MAC) {
                String cpuArch = SystemUtils.OS_ARCH;
                String x86 = "x86", arm = "arm";
                if (cpuArch.contains(x86)) {
                    path = MavenIndexerBinTypeEnum.MAC_AMD.getPath();
                } else if (cpuArch.contains(arm)) {
                    path = MavenIndexerBinTypeEnum.MAC_ARM.getPath();
                }
            } else if (SystemUtils.IS_OS_WINDOWS) {
                path = MavenIndexerBinTypeEnum.WINDOWS.getPath();
            }
            ClassLoader classLoader = getClass().getClassLoader();
            String targetPath = tempPath + File.separator + UUID.randomUUID() + File.separator + path.substring(path.lastIndexOf("/") + 1);
            try (InputStream inputStream = classLoader.getResourceAsStream(path)) {
                if (Objects.nonNull(inputStream)) {
                    Path binPath = Path.of(targetPath);
                    Path parentDir = binPath.getParent();
                    if (parentDir != null && !Files.exists(parentDir)) {
                        Files.createDirectories(parentDir);
                    }
                    // 创建目标文件（如果不存在）
                    if (!Files.exists(binPath)) {
                        Files.createFile(binPath);
                    }
                    Files.copy(inputStream, binPath, StandardCopyOption.REPLACE_EXISTING);
                    // 设置目标文件的权限（示例中使用 POSIX 权限）
                    Set<PosixFilePermission> permissions = Sets.newHashSet();
                    permissions.add(PosixFilePermission.OWNER_READ);
                    permissions.add(PosixFilePermission.OWNER_WRITE);
                    permissions.add(PosixFilePermission.OWNER_EXECUTE);
                    Files.setPosixFilePermissions(binPath, permissions);
                }
            }
            log.info("获取解析MavenIndexer的脚本path [{}]", targetPath);
            return targetPath;
        } catch (Exception ex) {
            log.error("获取解析MavenIndexer的脚本错误 [{}]", ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException("获取解析MavenIndexer的脚本错误");
        }
    }

    /**
     * 记录下载状态
     *
     * @param key     key
     * @param value   value
     * @param comment comment
     */
    private void handlerDownLoadStatus(String key, String value, String comment) {
        Dict dict = Dict.builder().dictType(DictTypeEnum.HANDLER_MAVEN_INDEXER.getType()).dictKey(key).dictValue(value).comment(comment).build();
        dictService.saveOrUpdateDict(dict, true);
    }

    /**
     * build ThreadPoolTaskExecutor
     *
     * @param corePoolSize corePoolSize
     * @param maxPoolSize  maxPoolSize
     * @return ThreadPoolTaskExecutor
     */
    private ThreadPoolTaskExecutor buildThreadPoolTaskExecutor(Integer corePoolSize, Integer maxPoolSize) {
        ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
        executor.setCorePoolSize(corePoolSize);
        executor.setMaxPoolSize(maxPoolSize);
        executor.setQueueCapacity(1000000);
        executor.setKeepAliveSeconds(120);
        executor.setThreadNamePrefix("customSyncArtifact_");
        executor.setWaitForTasksToCompleteOnShutdown(true);
        executor.setAwaitTerminationSeconds(6);
        executor.setRejectedExecutionHandler(new ThreadPoolExecutor.CallerRunsPolicy());
        executor.initialize();
        return executor;
    }
}


