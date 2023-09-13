package com.veadan.folib.services.impl;

import cn.hutool.core.io.FileUtil;
import com.alibaba.fastjson.JSONObject;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.entity.Dict;
import com.veadan.folib.enums.DictTypeEnum;
import com.veadan.folib.enums.MavenIndexerBinTypeEnum;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.services.ArtifactResolutionService;
import com.veadan.folib.services.DictService;
import com.veadan.folib.services.MavenIndexerService;
import com.veadan.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.LineIterator;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.SystemUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.DefaultResourceLoader;
import org.springframework.core.io.Resource;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Service;

import javax.inject.Inject;
import java.io.File;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.attribute.PosixFilePermission;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.FutureTask;
import java.util.concurrent.atomic.AtomicLong;

/**
 * @author leipenghui
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
    private ThreadPoolTaskExecutor asyncDownloadArtifactThreadPoolTaskExecutor;

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
            String command = binPath + " --format " + format + "  --indexId " + indexId + " --chainId " + chainId + " --url " + url;
            Process process = Runtime.getRuntime().exec(command);
            //获取命令输出结果
            try (InputStream inputStream = process.getInputStream()) {
                FileUtil.writeFromStream(inputStream, file);
            }
            //等待命令执行完成
            process.waitFor();
            log.info("存储MavenIndexer format [{}] indexId [{}] chainId [{}] url [{}] 结束 targetPath [{}] 耗时约为 [{}] 秒", format, indexId, chainId, url, targetPath, (System.currentTimeMillis() - startTime) / 1000);
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
    public void handlerMavenIndexerAndDownLoad(Repository repository, String mavenIndexerPath, Integer batch) {
        if (Objects.isNull(batch)) {
            batch = 500;
        }
        File file = null;
        String dictKey = repository.getStorageIdAndRepositoryId(), storageId = repository.getStorage().getId(), repositoryId = repository.getId();
        JSONObject dictValueJson = new JSONObject();
        long lines = 0, validLines = 0, startTime = System.currentTimeMillis();
        AtomicLong al = new AtomicLong(0), successAl = new AtomicLong(0), failAl = new AtomicLong(0);
        try {
            file = new File(mavenIndexerPath);
            dictValueJson.put("mavenIndexerFileName", file.getName());
            if (FileUtil.isDirectory(file) || !FileUtil.exist(file)) {
                log.warn("MavenIndexer storageId [{}] repositoryId [{}] mavenIndexerPath [{}] 文件不存在", storageId, repositoryId, mavenIndexerPath);
                return;
            }
            List<String> fileExtensionList = Lists.newArrayList("pom", "jar", "war", "ear", "zip"), artifactPathList = Lists.newArrayList();
            try (LineIterator lineIterator = FileUtils.lineIterator(file, "UTF-8")) {
                String fileExtension, groupId, artifactId, version, artifactPath, repositoryUrl = String.format("%s/storages/%s/%s/", StringUtils.chomp(configurationManager.getConfiguration().getBaseUrl(), "/"), storageId, repositoryId), currentLine = "";
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
                            validLines++;
                            artifactPathList.add(artifactPath);
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
            log.info("开始同步MavenIndexer storageId [{}] repositoryId [{}] mavenIndexerPath [{}] batch [{}] 找到 [{}] 条匹配的制品数据", storageId, repositoryId, mavenIndexerPath, batch, artifactPathList.size());
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
                        } catch (Exception ex) {
                            log.warn("同步MavenIndexer storageId [{}] repositoryId [{}] artifactPath [{}] mavenIndexerPath [{}] 第 [{}] 个有效行, fail [{}]", storageId, repositoryId, itemArtifactPath, mavenIndexerPath, current, failAl.get());
                        }
                    }
                    return "success";
                });
                futureTaskList.add(futureTask);
                asyncDownloadArtifactThreadPoolTaskExecutor.submit(futureTask);
            }
            for (FutureTask<String> task : futureTaskList) {
                task.get();
            }
            dictValueJson.put("lines", lines);
            dictValueJson.put("validLines", validLines);
            dictValueJson.put("process", al.get());
            dictValueJson.put("success", successAl.get());
            dictValueJson.put("fail", failAl.get());
            dictValueJson.put("takeTime", System.currentTimeMillis() - startTime);
            handlerDownLoadStatus(dictKey, dictValueJson.toJSONString(), null);
        } catch (Exception ex) {
            log.error("同步MavenIndexer storageId [{}] repositoryId [{}] mavenIndexerPath [{}] 错误 [{}]", storageId, repositoryId, mavenIndexerPath, ExceptionUtils.getStackTrace(ex));
            dictValueJson.put("lines", lines);
            dictValueJson.put("validLines", validLines);
            dictValueJson.put("process", al.get());
            dictValueJson.put("success", successAl.get());
            dictValueJson.put("fail", failAl.get());
            dictValueJson.put("takeTime", System.currentTimeMillis() - startTime);
            handlerDownLoadStatus(dictKey, dictValueJson.toJSONString(), al.get() + "");
        } finally {
            log.info("同步MavenIndexer storageId [{}] repositoryId [{}] mavenIndexerPath [{}] 结束 lines [{}] validLines [{}] process [{}] fail [{}] success [{}] take time [{}]", storageId, repositoryId, mavenIndexerPath, lines, validLines, al.get(), failAl.get(), successAl.get(), System.currentTimeMillis() - startTime);
            if (Objects.nonNull(file)) {
                FileUtil.del(file.getParent());
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
     * @param key         key
     * @param value       value
     * @param currentLine currentLine
     */
    private void handlerDownLoadStatus(String key, String value, String currentLine) {
        Dict dict = Dict.builder().dictType(DictTypeEnum.HANDLER_MAVEN_INDEXER.getType()).dictKey(key).dictValue(value).comment(currentLine).build();
        dictService.saveOrUpdateDict(dict, true);
    }

    public static void main(String[] args) throws Exception {
        String p = "/Users/leipenghui/project/java/boyun/folib-server/folib-vault/tmp/20a57af5-fd71-4bf5-ab50-88f34dfadf12/local-maven_index.dump";
        File file = new File(p);
        LineIterator lineIterator = FileUtils.lineIterator(file, "UTF-8");
        long startTime = System.currentTimeMillis();
        long lines = 0, validLines = 0;
        List<String> fileExtensionList = Lists.newArrayList("pom", "jar", "war", "ear", "zip");
        String fileExtension, currentLine = "";
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
                if (fileExtensionList.contains(fileExtension)) {
                    validLines++;
                }
            } catch (Exception ex) {
            }
        }
        long endTime = System.currentTimeMillis() - startTime;
        System.out.println(String.format("总行数 [%s] 有效行数 [%s] ,耗时 [%s] 毫秒 [%s] 秒", lines, validLines, endTime, (endTime / 1000)));
    }
}


