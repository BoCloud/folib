package com.veadan.folib.storage.validation.resource;

import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.components.DistributedCacheComponent;
import com.veadan.folib.configuration.AlarmConfiguration;
import com.veadan.folib.configuration.Configuration;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.enums.FileUnitTypeEnum;
import com.veadan.folib.event.validator.ValidatorEvent;
import com.veadan.folib.event.validator.ValidatorEventListenerRegistry;
import com.veadan.folib.event.validator.ValidatorEventTypeEnum;
import com.veadan.folib.providers.ProviderImplementationException;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.layout.LayoutProvider;
import com.veadan.folib.providers.layout.LayoutProviderRegistry;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.storage.ArtifactResolutionException;
import com.veadan.folib.storage.ArtifactStorageException;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import com.veadan.folib.util.FileSizeConvertUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;
import org.springframework.web.multipart.MultipartFile;

import jakarta.inject.Inject;
import java.io.BufferedInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.channels.FileChannel;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.Collections;
import java.util.Objects;

/**
 * @author mtodorov
 */
@Slf4j
@Component("artifactOperationsValidator")
public class ArtifactOperationsValidator {

    @Lazy
    @Inject
    private ConfigurationManager configurationManager;
    @Lazy
    @Inject
    private LayoutProviderRegistry layoutProviderRegistry;
    @Lazy
    @Inject
    private RepositoryPathResolver repositoryPathResolver;
    @Lazy
    @Inject
    private ArtifactRepository artifactRepository;
    @Lazy
    @Inject
    private DistributedCacheComponent distributedCacheComponent;
    @Lazy
    @Inject
    private ValidatorEventListenerRegistry validatorEventListenerRegistry;

    private static final long MINUTES_TO_MILLIS = 1L;

    public ArtifactOperationsValidator() {
    }

    public void validate(RepositoryPath repositoryPath)
            throws ArtifactResolutionException {
        checkArtifactPath(repositoryPath);

        Repository repository = repositoryPath.getRepository();
        Storage storage = repository.getStorage();

        checkStorageExists(storage.getId());
        checkRepositoryExists(storage.getId(), repository.getId());
    }

    public void checkStorageExists(String storageId)
            throws ArtifactResolutionException {
        if (storageId == null) {
            throw new ArtifactResolutionException("No storage specified.");
        }

        if (getConfiguration().getStorage(storageId) == null) {
            throw new ArtifactResolutionException("Storage " + storageId + " does not exist.");
        }
    }

    public void checkRepositoryExists(String storageId,
                                      String repositoryId)
            throws ArtifactResolutionException {
        if (repositoryId == null) {
            throw new ArtifactResolutionException("No repository specified.");
        }

        if (getConfiguration().getStorage(storageId)
                .getRepository(repositoryId) == null) {
            throw new ArtifactResolutionException("Repository " + repositoryId + " does not exist.");
        }
    }

    public void checkArtifactPath(RepositoryPath repositoryPath)
            throws ArtifactResolutionException {
        if (repositoryPath == null) {
            throw new ArtifactResolutionException("No artifact path specified.");
        }
    }

    public void checkAllowsDeployment(Repository repository)
            throws ArtifactStorageException {
        if (!repository.isAllowsDeployment() ||
                RepositoryTypeEnum.GROUP.getType().equals(repository.getType()) ||
                RepositoryTypeEnum.PROXY.getType().equals(repository.getType())) {
            // It should not be possible to write artifacts to:
            // - a repository that doesn't allow the deployment of artifacts
            // - a proxy repository
            // - a group repository
            //
            // NOTE:
            // - A proxy repository should only serve artifacts that already exist in the cache, or the remote host.
            // - Both the ProxyRepositoryProvider and GroupRepositoryProvider need to have an implementation of the
            //   getOutputStream(...) method, which is why this check is performed here instead.

            throw new ArtifactStorageException("Deployment of artifacts to " + repository.getType() +
                    " repositories is not allowed!");
        }
    }

    public void checkAllowsRedeployment(Repository repository,
                                        ArtifactCoordinates coordinates)
            throws IOException,
            ProviderImplementationException {
        LayoutProvider layoutProvider = LayoutProviderRegistry.getLayoutProvider(repository, layoutProviderRegistry);

        RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository, coordinates);
        if (RepositoryFiles.artifactExists(repositoryPath) && !repository.isAllowsRedeployment()) {
            throw new ArtifactStorageException("Re-deployment of artifacts to " +
                    repository.getStorage().getId() + ":" + repository.getId() +
                    " repository is not allowed!");
        }
    }

    public void checkAllowsDeletion(Repository repository)
            throws ArtifactStorageException {
        if (!repository.isAllowsDeletion()) {
            throw new ArtifactStorageException("Deleting artifacts from " + repository.getType() +
                    " repository is not allowed!");
        }
    }

    public void checkArtifactSize(String storageId,
                                  String repositoryId,
                                  MultipartFile uploadedFile)
            throws ArtifactResolutionException {
        if (uploadedFile.isEmpty() || uploadedFile.getSize() == 0) {
            throw new ArtifactResolutionException("Uploaded file is empty.");
        }

        Repository repository = getConfiguration().getStorage(storageId).getRepository(repositoryId);
        long artifactMaxSize = repository.getArtifactMaxSize();

        if (artifactMaxSize > 0 && uploadedFile.getSize() > artifactMaxSize) {
            throw new ArtifactResolutionException("The size of the artifact exceeds the maximum size accepted by " +
                    "this repository (" + uploadedFile.getSize() + "/" +
                    artifactMaxSize + ").");
        }
    }

    public void checkStorageSize(RepositoryPath repositoryPath)
            throws IOException {
        String storageId = repositoryPath.getStorageId();
        Storage storage = getConfiguration().getStorage(storageId);
        Long storageMaxSize = storage.getStorageMaxSize();
        if (Objects.isNull(storageMaxSize) || storageMaxSize <= 0) {
            return;
        }
        String STORAGE_SIZE_VERIFICATION_INTERVAL_KEY = "STORAGE_SIZE_VERIFICATION_INTERVAL";
        String STORAGE_SIZE_VERIFICATION_LAST_TIME_KEY = "STORAGE_SIZE_VERIFICATION_LAST_TIME";
        if (!isRefresh(STORAGE_SIZE_VERIFICATION_LAST_TIME_KEY, STORAGE_SIZE_VERIFICATION_INTERVAL_KEY)) {
            return;
        }
        long storageBytesSize = artifactRepository.artifactsBytesStatisticsByStorageIds(Collections.singletonList(storageId));
        log.info("The size [{}] of the storage [{}]", storageBytesSize, storageId);
        BigDecimal storageMaxTbSize = FileSizeConvertUtils.convertBytesWithDecimal(storageMaxSize, FileUnitTypeEnum.TB.getUnit());
        BigDecimal storageRealTbSize = FileSizeConvertUtils.convertBytesWithDecimal(storageBytesSize, FileUnitTypeEnum.TB.getUnit());
        double threshold = 0.90;
        AlarmConfiguration alarmConfiguration = getConfiguration().getAlarmConfiguration();
        if(alarmConfiguration.getStorageThreshold()>0){
            threshold = alarmConfiguration.getStorageThreshold();
        }
        BigDecimal useStorageProportion = storageRealTbSize.divide(storageMaxTbSize, 4, RoundingMode.HALF_UP).multiply(BigDecimal.valueOf(100));
        if (useStorageProportion.compareTo(BigDecimal.valueOf(threshold*100)) >= 0) {
            removeLastTime(STORAGE_SIZE_VERIFICATION_LAST_TIME_KEY);
            alarmNotification();
            throw new ArtifactResolutionException(String.format("The size of the storage [%s] artifact [%s] exceeds the maximum size accepted by " +
                    "this storage (%s/%s) unit %s.", storageId, repositoryPath, storageRealTbSize, storageMaxTbSize, FileUnitTypeEnum.TB.getUnit()));
        }
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
            return 1;
        }
        // 将获取的刷新间隔字符串解析为整数并返回
        return Integer.parseInt(refreshContentInterval);
    }

    /**
     * 设置最后一次刷新时间
     * @param key key
     * @param lastTime 最后一次刷新时间
     */
    public void setLastTime(String key, long lastTime) {
        distributedCacheComponent.put(key, Long.toString(lastTime));
    }

    /**
     * 删除最后一次刷新时间
     * @param key key
     */
    public void removeLastTime(String key) {
        distributedCacheComponent.delete(key);
    }

    /**
     * 获取最后一次刷新时间
     * @param key key
     * @return 最后一次刷新时间
     */
    public Long getLastTime(String key) {
        String lastTime = distributedCacheComponent.get(key);
        if (StringUtils.isBlank(lastTime)) {
            return null;
        }
        return Long.parseLong(lastTime);
    }

    /**
     * 判断是否需要刷新存储统计数据
     * 该方法通过比较当前时间与上次刷新时间，来决定是否需要进行刷新
     * 如果上次刷新时间为空，则自动设置当前时间为新的刷新时间，并返回true表示需要刷新
     * 如果当前时间与上次刷新时间的时间差大于等于预设的刷新间隔时间，则进行刷新并更新刷新时间
     * 否则，返回false表示不需要刷新
     * @param key key
     * @param intervalKey intervalKey
     * @return true，如果需要刷新缓存统计数据；否则返回false
     */
    public boolean isRefresh(String key, String intervalKey) {
        // 获取当前时间的瞬时值
        Instant now = Instant.now();
        // 获取上次刷新时间的毫秒值
        Long pastTimeMilli = getLastTime(key);
        // 如果上次刷新时间为空，则设置当前时间为新的刷新时间，并返回true表示需要刷新
        if (pastTimeMilli == null) {
            setLastTime(key, now.toEpochMilli());
            return true;
        }
        // 将上次刷新时间的毫秒值转换为瞬时值
        Instant pastTime = Instant.ofEpochMilli(pastTimeMilli);
        // 计算当前时间与上次刷新时间之间的时间差
        Duration duration = Duration.between(pastTime, now);
        // 计算刷新间隔时间的毫秒值
        long requiredMillis = refreshContentInterval(intervalKey) * MINUTES_TO_MILLIS;
        // 如果时间差大于等于刷新间隔时间，则进行刷新并更新刷新时间
        if (duration.compareTo(Duration.ofMillis(requiredMillis)) >= 0) {
            setLastTime(key, now.toEpochMilli());
            return true;
        }
        // 不需要刷新，返回false
        return false;
    }

    public Configuration getConfiguration() {
        return configurationManager.getConfiguration();
    }

//public long calculateStreamSize(InputStream inputStream) {
//       try (BufferedInputStream bis = new BufferedInputStream(inputStream)) {
//           byte[] buffer = new byte[8192];  // 使用 8KB 的缓冲区
//           int bytesRead;
//           long totalBytes = 0;
//           while ((bytesRead = bis.read(buffer)) != -1) {
//               totalBytes += bytesRead;  // 累加已读取的字节数
//           }
//           return totalBytes;
//       } catch (IOException e) {
//           log.error("Error calculating stream size: " + e.getMessage());
//       }
//       return 0;
//   }

   //  public long calculateStreamSize(InputStream inputStream) throws IOException {
   //    if (!inputStream.markSupported()) {
   //        throw new IllegalArgumentException("InputStream does not support mark/reset");
   //    }
   //    inputStream.mark(Integer.MAX_VALUE);
   //    try (BufferedInputStream bis = new BufferedInputStream(inputStream)) {
   //        byte[] buffer = new byte[8192];  // 使用 8KB 的缓冲区
   //        int bytesRead;
   //        long totalBytes = 0;
   //        while ((bytesRead = bis.read(buffer)) != -1) {
   //            totalBytes += bytesRead;  // 累加已读取的字节数
   //        }
   //        return totalBytes;
   //    } finally {
   //        inputStream.reset();
   //    }
   //}

    public long calculateStreamSize(InputStream inputStream) throws IOException {
        // 使用 BufferedInputStream 包装 inputStream，但不要关闭原始流
        BufferedInputStream bis = new BufferedInputStream(inputStream);

        try {
            // 检查 BufferedInputStream 是否支持 mark/reset
            if (!bis.markSupported()) {
                throw new IllegalArgumentException("InputStream does not support mark/reset");
            }

            // 标记当前位置，读取数据后可以通过 reset 恢复
            bis.mark(Integer.MAX_VALUE);

            byte[] buffer = new byte[8192];  // 使用 8KB 的缓冲区
            int bytesRead;
            long totalBytes = 0;

            // 读取并计算流的大小
            while ((bytesRead = bis.read(buffer)) != -1) {
                totalBytes += bytesRead;
            }

            return totalBytes;
        } finally {
            // 仅关闭 BufferedInputStream，而不是原始 inputStream
            bis.close();
        }
    }



    public long getFileSize(String filePath) throws IOException {
        try (FileChannel fileChannel = new FileInputStream(filePath).getChannel()) {
            return fileChannel.size();
        }
    }

    /**
     * 检查仓库存储大小
     * @param repositoryPath 仓库路径
     * @throws IOException 异常
     */
    public void checkRepositorySize(RepositoryPath repositoryPath)
            throws IOException {
        String storageId = repositoryPath.getStorageId();
        AlarmConfiguration alarmConfiguration = getConfiguration().getAlarmConfiguration();
        Repository repository = getConfiguration().getStorage(storageId).getRepository(repositoryPath.getRepositoryId());
        Long storageMaxSize = repository.getStorageMaxSize();
        if (Objects.isNull(storageMaxSize) || storageMaxSize <= 0) {
            return;
        }
        String REPOSITORY_SIZE_VERIFICATION_INTERVAL_KEY = "REPOSITORY_SIZE_VERIFICATION_INTERVAL";
        String REPOSITORY_SIZE_VERIFICATION_LAST_TIME_KEY = "REPOSITORY_SIZE_VERIFICATION_LAST_TIME";
        if (!isRefresh(REPOSITORY_SIZE_VERIFICATION_LAST_TIME_KEY, REPOSITORY_SIZE_VERIFICATION_INTERVAL_KEY)) {
            return;
        }
        long storageBytesSize = artifactRepository.artifactsBytesStatistics(Collections.singletonList(String.format("%s-%s", storageId, repositoryPath.getRepositoryId())));

        log.info("The size [{}] of the repository [{}/{}]", storageBytesSize, storageId,repositoryPath.getRepositoryId());
        BigDecimal storageMaxGbSize = FileSizeConvertUtils.convertBytesWithDecimal(storageMaxSize, FileUnitTypeEnum.GB.getUnit());
        BigDecimal storageRealGbSize = FileSizeConvertUtils.convertBytesWithDecimal(storageBytesSize, FileUnitTypeEnum.GB.getUnit());
        double threshold = 0.90;
        if(repository.getStorageThreshold() > 0 ){
            threshold = repository.getStorageThreshold();
        }else if(alarmConfiguration.getStorageThreshold()>0){
            threshold = alarmConfiguration.getStorageThreshold();
        }
        BigDecimal useStorageProportion = storageRealGbSize.divide(storageMaxGbSize, 4, RoundingMode.HALF_UP).multiply(BigDecimal.valueOf(100));
        if (useStorageProportion.compareTo(BigDecimal.valueOf(threshold*100)) >= 0) {
            removeLastTime(REPOSITORY_SIZE_VERIFICATION_LAST_TIME_KEY);
            alarmNotification();
            throw new ArtifactResolutionException(String.format("The size of the repository [%s/%s] artifact [%s] exceeds the maximum size accepted by " +
                    "this storage (%s/%s) unit %s.", storageId,repositoryPath.getRepositoryId(), repositoryPath, storageRealGbSize, storageMaxGbSize, FileUnitTypeEnum.GB.getUnit()));
        }
    }

    /**
     * 告警通知
     */
    public void alarmNotification() {
        String key = "NOTIFICATION_VALID_FROM";
        Long validFrom = this.getNotificationValidFrom(key);
        boolean flag = false;
        Instant now = Instant.now();
        if (validFrom == null) {
            flag = true;
        } else if (validFrom < now.getEpochSecond()) {
            flag = true;
        }
        if (flag) {
            setNotificationValidFrom(key, now);
            validatorEventListenerRegistry.dispatchEvent(new ValidatorEvent(ValidatorEventTypeEnum.STORAGE_VALIDATOR.getType()));
        }
    }

    /**
     * 设置通知有效时间
     * @param key
     * @param now
     */
    public void setNotificationValidFrom(String key, Instant now){
        distributedCacheComponent.put(key, Long.toString(now.plus(4, ChronoUnit.HOURS).getEpochSecond()));
        //distributedCacheComponent.put(key, Long.toString(now.plus(1, ChronoUnit.MINUTES).getEpochSecond()));
    }

    public Long getNotificationValidFrom(String key){
        String value = distributedCacheComponent.get(key);
        if(Objects.isNull(value)){
            return null;
        }
        return Long.valueOf(value);
    }
}
