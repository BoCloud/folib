package com.veadan.folib.eventlistener.artifactcache;

import com.google.common.collect.Lists;
import com.veadan.folib.components.DistributedCacheComponent;
import com.veadan.folib.components.artifact.ArtifactComponent;
import com.veadan.folib.domain.CacheSettings;
import com.veadan.folib.entity.ArtifactCacheRecord;
import com.veadan.folib.event.AsyncEventListener;
import com.veadan.folib.event.artifact.ArtifactEvent;
import com.veadan.folib.event.artifact.ArtifactEventTypeEnum;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.services.ArtifactCacheRecordService;
import com.veadan.folib.storage.metadata.MetadataHelper;
import com.veadan.folib.util.FileSizeConvertUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.time.Duration;
import java.time.Instant;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicLong;

/**
 * @author leipenghui
 * 事件监听，处理制品缓存
 **/
@Slf4j
@Component
public class ArtifactEventCacheListener {

    @Inject
    private ArtifactComponent artifactComponent;

    @Inject
    private ArtifactCacheRecordService artifactCacheRecordService;

    @Inject
    private DistributedCacheComponent distributedCacheComponent;

    private final String REFRESH_CACHE_STATISTICS_KEY = "ARTIFACT_CACHE_VERIFICATION_INTERVAL";

    private final int ARTIFACT_CACHE_VERIFICATION_INTERVAL = 360;

    private final String ARTIFACT_CACHE_LAST_TIME = "ARTIFACT_CACHE_VERIFICATION_LAST_TIME";

    private static final long MINUTES_TO_MILLIS = 60_000L;


    @AsyncEventListener
    public void handle(final ArtifactEvent<RepositoryPath> event) {
        long startTime = System.currentTimeMillis();
        int source = (int) event.getSource();
        RepositoryPath repositoryPath = event.getPath();
        ArtifactEventTypeEnum artifactEventTypeEnum = ArtifactEventTypeEnum.queryArtifactEventTypeEnumByType(source);
        log.debug("监听到制品事件 [{}]，path路径 [{}]", artifactEventTypeEnum, repositoryPath);
        if (Objects.isNull(artifactEventTypeEnum)) {
            return;
        }
        if (!validateArtifactEvent(artifactEventTypeEnum)) {
            return;
        }
        try {
            if (repositoryPath.toString().contains(MetadataHelper.MAVEN_METADATA_XML)) {
                return;
            }
            if (ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_CACHE.getType() == artifactEventTypeEnum.getType() && !artifactExists(repositoryPath)) {
                return;
            }
            String sourcePath = repositoryPath.toString();
            String storageId = repositoryPath.getStorageId(), repositoryId = repositoryPath.getRepositoryId();
            String prefix = String.format("/%s/%s/", storageId, repositoryId);
            String targetSubPath = sourcePath.substring(sourcePath.indexOf(prefix) + 1);
            String artifactPath = sourcePath.substring(sourcePath.indexOf(prefix) + prefix.length());
            if (ArtifactEventTypeEnum.EVENT_ARTIFACT_PATH_DELETED.getType() == artifactEventTypeEnum.getType() || ArtifactEventTypeEnum.EVENT_ARTIFACT_DIRECTORY_PATH_DELETED.getType() == artifactEventTypeEnum.getType()) {
                artifactPath = ArtifactEventTypeEnum.EVENT_ARTIFACT_DIRECTORY_PATH_DELETED.getType() == artifactEventTypeEnum.getType() ? artifactPath + "/" : artifactPath;
                ArtifactCacheRecord artifactCacheRecord = ArtifactCacheRecord.builder().storageId(repositoryPath.getStorageId()).repositoryId(repositoryPath.getRepositoryId())
                        .artifactPath(artifactPath).build();
                int limit = artifactCacheRecordService.getArtifactCacheRecordCount(artifactCacheRecord);
                List<ArtifactCacheRecord> artifactCacheRecordList = artifactComponent.getArtifactCacheRecord(artifactCacheRecord, limit);
                if (CollectionUtils.isNotEmpty(artifactCacheRecordList)) {
                    List<List<ArtifactCacheRecord>> deleteIdList = Lists.partition(artifactCacheRecordList, 200);
                    for (List<ArtifactCacheRecord> itemList : deleteIdList) {
                        for (ArtifactCacheRecord deleteArtifactCacheRecord : itemList) {
                            artifactCacheRecordService.deleteArtifactCacheRecord(deleteArtifactCacheRecord);
                        }
                    }
                }
                return;
            }
            CacheSettings cacheSettings = artifactComponent.getCacheConfig();
            if (Objects.isNull(cacheSettings) || !cacheSettings.isEnabled()) {
                return;
            }
            Path backupPath = Files.createDirectories(Paths.get(cacheSettings.getDirectoryPath()));
            Path targetPath = backupPath.resolve(targetSubPath);
            if (ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_UPDATED.getType() == artifactEventTypeEnum.getType() && Files.notExists(targetPath)) {
                return;
            }
            log.debug("缓存功能已开启 storageId [{}] repositoryId [{}]，源制品地址 [{}] 缓存制品地址 [{}]", storageId, repositoryId, sourcePath, targetPath.toString());
            String minSize = cacheSettings.getMinSize(), maxSize = cacheSettings.getMaxSize();
            if (StringUtils.isNotBlank(minSize)) {
                String minSizeUnit = cacheSettings.getMinSizeUnit();
                long size = Files.size(repositoryPath);
                BigDecimal convertSize = FileSizeConvertUtils.convertBytesWithDecimal(size, minSizeUnit);
                if (convertSize.compareTo(new BigDecimal(minSize)) < 0) {
                    log.debug("缓存功能已开启 storageId [{}] repositoryId [{}] byteSize [{}] convertSize [{}] beginSize [{}] [{}]源制品小于单文件最小缓存值不满足缓存条件", storageId, repositoryId, size, convertSize, minSize, minSizeUnit);
                    return;
                }
            }
            if (StringUtils.isNotBlank(maxSize)) {
                String maxSizeUnit = cacheSettings.getMaxSizeUnit();
                long size = Files.size(repositoryPath);
                BigDecimal convertSize = FileSizeConvertUtils.convertBytesWithDecimal(size, maxSizeUnit);
                if (convertSize.compareTo(new BigDecimal(maxSize)) > 0) {
                    log.debug("缓存功能已开启 storageId [{}] repositoryId [{}] byteSize [{}] convertSize [{}] beginSize [{}] [{}]源制品大于单文件最大缓存值不满足缓存条件", storageId, repositoryId, size, convertSize, maxSize, maxSizeUnit);
                    return;
                }
            }

            try {
                if (isRefresh()) {
                    log.info("缓存功能已开启，缓存容量 [{}] [{}] 开始校验是否需要清理缓存", cacheSettings.getSize(), cacheSettings.getSizeUnit());
                    BigDecimal oneHundred = BigDecimal.valueOf(100);
                    int clearCondition = cacheSettings.getClearCondition();
                    //long cacheDirectoryPathUseSize = FileUtils.sizeOfDirectory(new File(cacheSettings.getDirectoryPath()));
                    long cacheDirectoryPathUseSize = getDirectorySize(Path.of(cacheSettings.getDirectoryPath()));
                    //加上当前的制品大小
                    long cacheDirectoryPathAllSize = cacheDirectoryPathUseSize + Files.size(repositoryPath);
                    BigDecimal cacheDirectoryPathConvertSize = FileSizeConvertUtils.convertBytesWithDecimal(cacheDirectoryPathUseSize, cacheSettings.getSizeUnit());
                    BigDecimal cacheDirectoryPathProportion = cacheDirectoryPathConvertSize.divide(new BigDecimal(cacheSettings.getSize()), 4, RoundingMode.HALF_UP).multiply(oneHundred);

                    BigDecimal cacheDirectoryPathConvertAllSize = FileSizeConvertUtils.convertBytesWithDecimal(cacheDirectoryPathAllSize, cacheSettings.getSizeUnit());
                    BigDecimal cacheDirectoryPathAllProportion = cacheDirectoryPathConvertAllSize.divide(new BigDecimal(cacheSettings.getSize()), 4, RoundingMode.HALF_UP).multiply(oneHundred);

                    int clearProportion = cacheSettings.getClearProportion();
                    long clearBytes = FileSizeConvertUtils.convertToBytes(clearProportion, cacheSettings.getSizeUnit());
                    log.debug("缓存功能已开启，缓存容量 [{}] [{}] 当前已缓存制品 [{}] 字节，约为[{}] [{}]，占用缓存比为 [{}%]，加上当前制品后为 [{}] 字节，约为 [{}] [{}] 占用缓存比为 [{}%]", cacheSettings.getSize(), cacheSettings.getSizeUnit(), cacheDirectoryPathUseSize, cacheDirectoryPathConvertSize, cacheSettings.getSizeUnit(), cacheDirectoryPathProportion, cacheDirectoryPathAllSize, cacheDirectoryPathConvertAllSize, cacheSettings.getSizeUnit(), cacheDirectoryPathAllProportion);
                    if (cacheDirectoryPathAllProportion.compareTo(oneHundred) >= 0) {
                        log.warn("缓存功能已开启，缓存容量 [{}] [{}] 当前已缓存制品 [{}] 字节，约为[{}] [{}]，占用缓存比为 [{}%]，加上当前制品后为 [{}] 字节，约为 [{}] [{}] 占用缓存比为 [{}%]，大于总容量，禁止写入", cacheSettings.getSize(), cacheSettings.getSizeUnit(), cacheDirectoryPathUseSize, cacheDirectoryPathConvertSize, cacheSettings.getSizeUnit(), cacheDirectoryPathProportion, cacheDirectoryPathAllSize, cacheDirectoryPathConvertAllSize, cacheSettings.getSizeUnit(), cacheDirectoryPathAllProportion);
                        return;
                    }
                    if (cacheDirectoryPathAllProportion.compareTo(new BigDecimal(clearCondition)) >= 0) {
                        log.debug("缓存功能已开启，缓存容量 [{}] [{}] 当前已缓存制品 [{}] 字节，约为[{}] [{}]，占用缓存比为 [{}%]，加上当前制品后为 [{}] 字节，约为 [{}] [{}] 占用缓存比为 [{}%]，已达到清理条件 [{}%]", cacheSettings.getSize(), cacheSettings.getSizeUnit(), cacheDirectoryPathUseSize, cacheDirectoryPathConvertSize, cacheSettings.getSizeUnit(), cacheDirectoryPathProportion, cacheDirectoryPathAllSize, cacheDirectoryPathConvertAllSize, cacheSettings.getSizeUnit(), cacheDirectoryPathAllProportion, clearCondition);
                        long deleteBytes = 0L;
                        cleanup(clearBytes, deleteBytes, cacheSettings, cacheDirectoryPathUseSize, cacheDirectoryPathConvertSize, cacheDirectoryPathProportion, cacheDirectoryPathAllSize, cacheDirectoryPathConvertAllSize, cacheDirectoryPathAllProportion);
                    }
                }
                Files.createDirectories(targetPath.getParent());
                //缓存制品
                Files.copy(repositoryPath.getTarget(), targetPath, StandardCopyOption.REPLACE_EXISTING);
                if (RepositoryFiles.isArtifact(repositoryPath)) {
                    //缓存checksum
                    repositoryPath.getFileSystem().provider().resolveChecksumPathMap(repositoryPath).forEach((key, value) -> {
                        try {
                            Path checksumPath = targetPath.getParent().resolve(FilenameUtils.getName(value.toString()));
                            Files.copy(value, checksumPath, StandardCopyOption.REPLACE_EXISTING);
                        } catch (FileAlreadyExistsException e) {
                            //destination file already exists
                        } catch (Exception ex) {
                            log.warn("缓存制品checksumPath [{}] [{}] [{}] 错误 [{}]", storageId, repositoryId, repositoryPath.toString(), ExceptionUtils.getStackTrace(ex));
                        }
                    });
                }
                //缓存metadata
                artifactComponent.storeArtifactMetadataFile(repositoryPath, targetPath);
                artifactComponent.handlerArtifactCacheRecord(repositoryPath, cacheSettings, targetPath);
                log.info("Handle artifact cache storageId [{}] repositoryId [{}] artifactPath [{}] take time [{}] ms", storageId, repositoryId, targetSubPath, System.currentTimeMillis() - startTime);
            } catch (FileAlreadyExistsException e) {
                //destination file already exists
            } catch (Exception e) {
                log.warn("处理制品缓存错误 [{}] 错误：[{}]", repositoryPath.toString(), ExceptionUtils.getStackTrace(e));
            }
        } catch (Exception ex) {
            log.error("事件监听，处理backup，事件类型：{} repositoryPath：{} 错误：{}", source, repositoryPath, ExceptionUtils.getStackTrace(ex));
        }
    }

    /**
     * 校验制品事件类型是否为需要处理的类型
     *
     * @param artifactEventTypeEnum 制品事件类型
     * @return true 需要处理 false 不需要处理
     */
    private boolean validateArtifactEvent(ArtifactEventTypeEnum artifactEventTypeEnum) {
        List<Integer> list = Arrays.asList(ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_CACHE.getType(), ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_UPDATED.getType(), ArtifactEventTypeEnum.EVENT_ARTIFACT_PATH_DELETED.getType(), ArtifactEventTypeEnum.EVENT_ARTIFACT_DIRECTORY_PATH_DELETED.getType());
        return list.contains(artifactEventTypeEnum.getType());
    }

    /**
     * 制品存在判断
     *
     * @param repositoryPath 制品对象
     * @return true 存在 false 不存在
     */
    public boolean artifactExists(RepositoryPath repositoryPath) {
        if (Objects.isNull(repositoryPath) || !Files.exists(repositoryPath)) {
            log.error("RepositoryPath [{}] does not exist", repositoryPath);
            return false;
        }
        return true;
    }

    private void cleanup(Long clearBytes, Long deleteBytes, CacheSettings cacheSettings,
                         long cacheDirectoryPathUseSize, BigDecimal cacheDirectoryPathConvertSize, BigDecimal
                                 cacheDirectoryPathProportion, long cacheDirectoryPathAllSize, BigDecimal
                                 cacheDirectoryPathConvertAllSize, BigDecimal cacheDirectoryPathAllProportion) {
        List<ArtifactCacheRecord> artifactCacheRecordList = artifactComponent.getArtifactCacheRecord(null, 5);
        if (CollectionUtils.isNotEmpty(artifactCacheRecordList)) {
            BigDecimal deleteProportion;
            Path artifactCachePath = null;
            for (ArtifactCacheRecord artifactCacheRecord : artifactCacheRecordList) {
                artifactCachePath = Path.of(artifactCacheRecord.getCachePath());
                if (!Files.exists(artifactCachePath)) {
                    artifactCacheRecordService.deleteArtifactCacheRecord(artifactCacheRecord);
                    continue;
                }
                artifactCacheRecordService.deleteArtifactCacheRecord(artifactCacheRecord);
                deleteBytes = deleteBytes + artifactCacheRecord.getSize();
                deleteProportion = BigDecimal.valueOf(deleteBytes).divide(BigDecimal.valueOf(clearBytes), 4, RoundingMode.HALF_UP).multiply(BigDecimal.valueOf(100));
                log.debug("缓存功能已开启，缓存容量 [{}] [{}] 当前已缓存制品 [{}] 字节，约为[{}] [{}]，占用缓存比为 [{}%]，加上当前制品后为 [{}] 字节，约为 [{}] [{}] 占用缓存比为 [{}%]，已达到清理条件 [{}%]，缓存制品 [{}] 已清除，释放了 [{}] 字节，需释放 [{}] 字节 已释放总 [{}] 字节，已释放百分比为 [{}%]", cacheSettings.getSize(), cacheSettings.getSizeUnit(), cacheDirectoryPathUseSize, cacheDirectoryPathConvertSize, cacheSettings.getSizeUnit(), cacheDirectoryPathProportion, cacheDirectoryPathAllSize, cacheDirectoryPathConvertAllSize, cacheSettings.getSizeUnit(), cacheDirectoryPathAllProportion, cacheSettings.getClearCondition(), artifactCacheRecord.getCachePath(), artifactCacheRecord.getSize(), clearBytes, deleteBytes, deleteProportion);
                if (deleteBytes >= clearBytes) {
                    log.debug("缓存功能已开启，缓存容量 [{}] [{}] 当前已缓存制品 [{}] 字节，约为[{}] [{}]，占用缓存比为 [{}%]，加上当前制品后为 [{}] 字节，约为 [{}] [{}] 占用缓存比为 [{}%]，已达到清理条件 [{}%]，缓存制品 [{}] 已清除，释放了 [{}] 字节，需释放 [{}] 字节 已释放总 [{}] 字节，已释放百分比为 [{}%]，已释放字节大于等于需释放字节，清理结束", cacheSettings.getSize(), cacheSettings.getSizeUnit(), cacheDirectoryPathUseSize, cacheDirectoryPathConvertSize, cacheSettings.getSizeUnit(), cacheDirectoryPathProportion, cacheDirectoryPathAllSize, cacheDirectoryPathConvertAllSize, cacheSettings.getSizeUnit(), cacheDirectoryPathAllProportion, cacheSettings.getClearCondition(), artifactCacheRecord.getCachePath(), artifactCacheRecord.getSize(), clearBytes, deleteBytes, deleteProportion);
                    return;
                }
            }
            if (deleteBytes < clearBytes) {
                cleanup(clearBytes, deleteBytes, cacheSettings, cacheDirectoryPathUseSize, cacheDirectoryPathConvertSize, cacheDirectoryPathProportion, cacheDirectoryPathAllSize, cacheDirectoryPathConvertAllSize, cacheDirectoryPathAllProportion);
            }
        } else {
            log.debug("缓存功能已开启，缓存容量 [{}] [{}] 当前已缓存制品 [{}] 字节，约为[{}] [{}]，占用缓存比为 [{}%]，加上当前制品后为 [{}] 字节，约为 [{}] [{}] 占用缓存比为 [{}%]，已达到清理条件 [{}%]，需释放 [{}] 字节 已释放总 [{}] 字节，已没有数据可以清理，清理结束", cacheSettings.getSize(), cacheSettings.getSizeUnit(), cacheDirectoryPathUseSize, cacheDirectoryPathConvertSize, cacheSettings.getSizeUnit(), cacheDirectoryPathProportion, cacheDirectoryPathAllSize, cacheDirectoryPathConvertAllSize, cacheSettings.getSizeUnit(), cacheDirectoryPathAllProportion, cacheSettings.getClearCondition(), clearBytes, deleteBytes);
        }
    }

    /**
     * 获取文件夹大小
     *
     * @param path 文件夹路径
     * @return 文件夹大小
     * @throws IOException 异常
     */
    public long getDirectorySize(Path path) throws IOException {
        final AtomicLong size = new AtomicLong(0);
        Files.walkFileTree(path, new SimpleFileVisitor<Path>() {
            @Override
            public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) {
                size.addAndGet(attrs.size());
                return FileVisitResult.CONTINUE;
            }
        });
        return size.get();
    }

    /**
     * 根据给定的键从分布式缓存中获取内容刷新间隔设置
     * 如果没有找到对应的值或者值为空，则返回默认的内容刷新间隔
     *
     * @param key 用于从分布式缓存中检索刷新间隔设置的键
     * @return 刷新间隔设置，如果未找到或值为空，则返回默认值
     */
    public int refreshContentInterval(final String key) {
        // 从分布式缓存中获取与给定键相关的刷新间隔设置
        String refreshContentInterval = distributedCacheComponent.get(key);

        // 如果获取的刷新间隔为空或仅为空白字符，则返回预设的默认刷新间隔
        if (StringUtils.isBlank(refreshContentInterval)) {
            return ARTIFACT_CACHE_VERIFICATION_INTERVAL;
        }

        // 将获取的刷新间隔字符串解析为整数并返回
        return Integer.parseInt(refreshContentInterval);
    }

    /**
     * 设置最后一次刷新时间
     *
     * @param lastTime 最后一次刷新时间
     */
    public void setLastTime(long lastTime) {
        distributedCacheComponent.put(ARTIFACT_CACHE_LAST_TIME, Long.toString(lastTime));
    }

    /**
     * 获取最后一次刷新时间
     *
     * @return 最后一次刷新时间
     */
    public Long getLastTime() {
        String lastTime = distributedCacheComponent.get(ARTIFACT_CACHE_LAST_TIME);
        if (StringUtils.isBlank(lastTime)) {
            return null;
        }
        return Long.parseLong(lastTime);
    }

    /**
     * 判断是否需要刷新缓存统计数据
     * 该方法通过比较当前时间与上次刷新时间，来决定是否需要进行刷新
     * 如果上次刷新时间为空，则自动设置当前时间为新的刷新时间，并返回true表示需要刷新
     * 如果当前时间与上次刷新时间的时间差大于等于预设的刷新间隔时间，则进行刷新并更新刷新时间
     * 否则，返回false表示不需要刷新
     *
     * @return true，如果需要刷新缓存统计数据；否则返回false
     */
    public boolean isRefresh() {

        // 获取当前时间的瞬时值
        Instant now = Instant.now();
        // 获取上次刷新时间的毫秒值
        Long pastTimeMilli = getLastTime();
        // 如果上次刷新时间为空，则设置当前时间为新的刷新时间，并返回true表示需要刷新
        if (pastTimeMilli == null) {
            setLastTime(now.toEpochMilli());
            return true;
        }
        // 将上次刷新时间的毫秒值转换为瞬时值
        Instant pastTime = Instant.ofEpochMilli(pastTimeMilli);
        // 计算当前时间与上次刷新时间之间的时间差
        Duration duration = Duration.between(pastTime, now);
        // 计算刷新间隔时间的毫秒值
        long requiredMillis = refreshContentInterval(REFRESH_CACHE_STATISTICS_KEY) * MINUTES_TO_MILLIS;
        // 如果时间差大于等于刷新间隔时间，则进行刷新并更新刷新时间
        if (duration.compareTo(Duration.ofMillis(requiredMillis)) >= 0) {
            setLastTime(now.toEpochMilli());
            return true;
        }
        // 不需要刷新，返回false
        return false;
    }

}
