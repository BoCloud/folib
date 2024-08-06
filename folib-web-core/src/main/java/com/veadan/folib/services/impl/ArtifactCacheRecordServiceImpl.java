package com.veadan.folib.services.impl;

import cn.hutool.core.date.StopWatch;
import com.github.pagehelper.PageHelper;
import com.hazelcast.core.HazelcastInstance;
import com.veadan.folib.components.artifact.ArtifactComponent;
import com.veadan.folib.db.schema.util.IpUtils;
import com.veadan.folib.domain.CacheSettings;
import com.veadan.folib.entity.ArtifactCacheRecord;
import com.veadan.folib.mapper.ArtifactCacheRecordMapper;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.services.ArtifactCacheRecordService;
import com.veadan.folib.util.FileSizeConvertUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import reactor.core.publisher.Flux;
import reactor.core.scheduler.Schedulers;
import tk.mybatis.mapper.entity.Example;

import javax.inject.Inject;
import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

/**
 * @author leipenghui
 * @date 2023/10/27
 **/
@Slf4j
@Service
public class ArtifactCacheRecordServiceImpl implements ArtifactCacheRecordService {

    @Inject
    private ArtifactCacheRecordMapper artifactCacheRecordMapper;

    @Inject
    @Lazy
    private ArtifactComponent artifactComponent;

    @Inject
    private HazelcastInstance hazelcastInstance;

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void saveArtifactCacheRecord(ArtifactCacheRecord artifactCacheRecord) {
        try {
            Date now = new Date();
            if (StringUtils.isNotBlank(artifactCacheRecord.getArtifactPath())) {
                int endIndex = artifactCacheRecord.getArtifactPath().length(), max = 768;
                if (endIndex > max) {
                    endIndex = max;
                }
                artifactCacheRecord.setArtifactPathPrefix(artifactCacheRecord.getArtifactPath().substring(0, endIndex));
            }
            artifactCacheRecord.setNodeId(getHostname());
            artifactCacheRecord.setId(hazelcastInstance.getFlakeIdGenerator("artifactCacheRecordId").newId());
            artifactCacheRecord.setCreateTime(now);
            artifactCacheRecord.setUpdateTime(now);
            artifactCacheRecord.setLatestDownloadTime(now);
            artifactCacheRecord.setDownloadCount(1L);
            artifactCacheRecordMapper.insertSelective(artifactCacheRecord);
        } catch (Exception ex) {
            deleteCacheFile(artifactCacheRecord.getCachePath());
            throw ex;
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateArtifactCacheRecord(ArtifactCacheRecord artifactCacheRecord) {
        ArtifactCacheRecord dbArtifactCacheRecord = selectOneArtifactCacheRecord(artifactCacheRecord);
        if (Objects.isNull(dbArtifactCacheRecord)) {
            return;
        }
        Date now = new Date();
        artifactCacheRecord.setId(dbArtifactCacheRecord.getId());
        artifactCacheRecord.setUpdateTime(now);
        artifactCacheRecord.setLatestDownloadTime(now);
        artifactCacheRecord.setDownloadCount(dbArtifactCacheRecord.getDownloadCount() + 1);
        artifactCacheRecordMapper.updateByPrimaryKeySelective(artifactCacheRecord);
    }

    @Transactional(rollbackFor = Exception.class)
    public void deleteArtifactCacheRecord(ArtifactCacheRecord artifactCacheRecord) {
        ArtifactCacheRecord dbArtifactCacheRecord = selectOneArtifactCacheRecord(artifactCacheRecord);
        if (Objects.isNull(dbArtifactCacheRecord)) {
            return;
        }
        try {
            boolean flag;
            Path artifactCachePath = Path.of(dbArtifactCacheRecord.getCachePath());
            if (!Files.exists(artifactCachePath)) {
                //缓存制品文件不存在，删除缓存checksum文件
                flag = handlerArtifactCacheDelete(artifactCachePath);
                if (flag) {
                    artifactCacheRecordMapper.deleteByPrimaryKey(dbArtifactCacheRecord.getId());
                }
                return;
            }
            //删除缓存制品文件
            flag = Files.deleteIfExists(artifactCachePath);
            if (flag) {
                //删除缓存checksum文件
                flag = handlerArtifactCacheDelete(artifactCachePath);
            }
            if (flag) {
                artifactCacheRecordMapper.deleteByPrimaryKey(dbArtifactCacheRecord.getId());
            }
        } catch (Exception ex) {
            if (ex instanceof NoSuchFileException) {
                artifactCacheRecordMapper.deleteByPrimaryKey(dbArtifactCacheRecord.getId());
                return;
            }
            log.error(ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException(ex.getMessage());
        }
    }
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void saveOrUpdateArtifactCacheRecord(ArtifactCacheRecord artifactCacheRecord) {
        ArtifactCacheRecord dbArtifactCacheRecord = selectOneArtifactCacheRecord(artifactCacheRecord);
        if (Objects.isNull(dbArtifactCacheRecord)) {
            saveArtifactCacheRecord(artifactCacheRecord);
        } else {
            updateArtifactCacheRecord(artifactCacheRecord);
        }
    }

    @Override
    public ArtifactCacheRecord selectOneArtifactCacheRecord(ArtifactCacheRecord artifactCacheRecord) {
        ArtifactCacheRecord resultArtifactCacheRecord = null;
        if (Objects.nonNull(artifactCacheRecord.getId())) {
            resultArtifactCacheRecord = artifactCacheRecordMapper.selectByPrimaryKey(artifactCacheRecord.getId());
        } else if (StringUtils.isNotBlank(artifactCacheRecord.getArtifactPath())) {
            Example example = Example.builder(ArtifactCacheRecord.class).build();
            Example.Criteria criteria = example.createCriteria();
            criteria.andEqualTo("nodeId", getHostname());
            criteria.andEqualTo("storageId", artifactCacheRecord.getStorageId());
            criteria.andEqualTo("repositoryId", artifactCacheRecord.getRepositoryId());
            criteria.andEqualTo("artifactPath", artifactCacheRecord.getArtifactPath());
            example.setOrderByClause("create_time desc");
            List<ArtifactCacheRecord> packageNameBlockList = artifactCacheRecordMapper.selectByExample(example);
            if (CollectionUtils.isNotEmpty(packageNameBlockList)) {
                resultArtifactCacheRecord = packageNameBlockList.get(0);
            }
        }
        return resultArtifactCacheRecord;
    }

    @Override
    public List<ArtifactCacheRecord> getArtifactCacheRecord(ArtifactCacheRecord artifactCacheRecord, Integer page, Integer limit) {
        Example example = null;
        if (Objects.nonNull(artifactCacheRecord)) {
            example = Example.builder(ArtifactCacheRecord.class).build();
            Example.Criteria criteria = example.createCriteria();
            criteria.andEqualTo("nodeId", getHostname());
            criteria.andEqualTo("storageId", artifactCacheRecord.getStorageId());
            criteria.andEqualTo("repositoryId", artifactCacheRecord.getRepositoryId());
            example.and().andLike("artifactPathPrefix", artifactCacheRecord.getArtifactPath() + "%");
        }
        if (Objects.isNull(page)) {
            page = 1;
        }
        if (Objects.isNull(limit)) {
            limit = 1000;
        }
        if (Objects.isNull(example)) {
            example = Example.builder(ArtifactCacheRecord.class).build();
            Example.Criteria criteria = example.createCriteria();
            criteria.andEqualTo("nodeId", getHostname());
        }
        example.setOrderByClause("latest_download_time asc, size desc");
        PageHelper.startPage(page, limit);
        return artifactCacheRecordMapper.selectByExample(example);
    }

    @Override
    public int getArtifactCacheRecordCount(ArtifactCacheRecord artifactCacheRecord) {
        Example example = Example.builder(ArtifactCacheRecord.class).build();
        Example.Criteria criteria = example.createCriteria();
        criteria.andEqualTo("nodeId", getHostname());
        if (Objects.nonNull(artifactCacheRecord)) {
            criteria.andEqualTo("storageId", artifactCacheRecord.getStorageId());
            criteria.andEqualTo("repositoryId", artifactCacheRecord.getRepositoryId());
            example.and().andLike("artifactPathPrefix", artifactCacheRecord.getArtifactPath() + "%");
            return artifactCacheRecordMapper.selectCountByExample(example);
        }
        return artifactCacheRecordMapper.selectCountByExample(example);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void deleteArtifactCacheRecordByIds(List<Long> idList) {
        if (CollectionUtils.isEmpty(idList)) {
            return;
        }
        artifactCacheRecordMapper.deleteByIds(idList.stream().map(String::valueOf).collect(Collectors.joining(",")));
    }

    @Override
    public void cleanupArtifactCacheDirectory(String directoryPath) throws Exception {
        File file = new File(directoryPath);
        if (file.exists()) {
            FileUtils.cleanDirectory(file);
            Example example = Example.builder(ArtifactCacheRecord.class).build();
            Example.Criteria criteria = example.createCriteria();
            criteria.andEqualTo("nodeId", getHostname());
            artifactCacheRecordMapper.deleteByExample(example);
        }
    }

    @Override
    public BigDecimal artifactCacheDirectoryUseSize(String directoryPath, String unit) throws Exception {
        BigDecimal size = BigDecimal.ZERO;
        if (StringUtils.isBlank(unit)) {
            unit = "GB";
        }
        Path path = Path.of(directoryPath);
        if (Files.exists(path)) {
            long bytesSize = FileUtils.sizeOfDirectory(path.toFile());
            size = FileSizeConvertUtils.convertBytesWithDecimal(bytesSize, unit);
        }
        return size;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void verifySourceRepositoryPath(RepositoryPath repositoryPath) {
        String sourcePath = repositoryPath.toString();
        String storageId = repositoryPath.getStorageId(), repositoryId = repositoryPath.getRepositoryId();
        String prefix = String.format("/%s/%s/", storageId, repositoryId);
        String artifactPath = sourcePath.substring(sourcePath.indexOf(prefix) + prefix.length());
        ArtifactCacheRecord artifactCacheRecord = ArtifactCacheRecord.builder().storageId(storageId).repositoryId(repositoryId).artifactPath(artifactPath).build();
        artifactCacheRecord = selectOneArtifactCacheRecord(artifactCacheRecord);
        if (Objects.nonNull(artifactCacheRecord) && !Files.exists(repositoryPath)) {
            deleteArtifactCacheRecord(artifactCacheRecord);
        } else if (!Files.exists(repositoryPath)) {
            try {
                CacheSettings cacheSettings = artifactComponent.getCacheConfig();
                if (Objects.isNull(cacheSettings)) {
                    return;
                }
                boolean flag;
                String cachePath = String.format("%s/%s/%s/%s", cacheSettings.getDirectoryPath(), storageId, repositoryId, artifactPath);
                Path artifactCachePath = Path.of(cachePath);
                if (!Files.exists(artifactCachePath)) {
                    //缓存制品文件不存在，删除缓存checksum文件
                    handlerArtifactCacheDelete(artifactCachePath);
                    return;
                }
                //删除缓存制品文件
                flag = Files.deleteIfExists(artifactCachePath);
                if (flag) {
                    //删除缓存checksum文件
                    handlerArtifactCacheDelete(artifactCachePath);
                }
            } catch (Exception ex) {
                log.error(ExceptionUtils.getStackTrace(ex));
                throw new RuntimeException(ex.getMessage());
            }
        }
    }

    private boolean handlerArtifactCacheDelete(Path artifactCachePath) {
        boolean flag = true;
        String fileName = artifactCachePath.getFileName().toString();
        if( artifactCachePath.getParent() == null){
            return flag;
        }
        try {
            String checksumMd5FileName = FilenameUtils.getName(fileName) + ".md5";
            Path checksumMd5CachePath = artifactCachePath.getParent().resolve(checksumMd5FileName);
            if (Files.exists(checksumMd5CachePath)) {
                flag = Files.deleteIfExists(checksumMd5CachePath);
            }
            String checksumSha1FileName = FilenameUtils.getName(fileName) + ".sha1";
            Path checksumSha1CachePath = artifactCachePath.getParent().resolve(checksumSha1FileName);
            if (Files.exists(checksumSha1CachePath)) {
                flag = Files.deleteIfExists(checksumSha1CachePath);
            }
            String checksumSha256FileName = FilenameUtils.getName(fileName) + ".sha256";
            Path checksumSha256CachePath = artifactCachePath.getParent().resolve(checksumSha256FileName);
            if (Files.exists(checksumSha256CachePath)) {
                flag = Files.deleteIfExists(checksumSha256CachePath);
            }
            String checksumSha512FileName = FilenameUtils.getName(fileName) + ".sha512";
            Path checksumSha512CachePath = artifactCachePath.getParent().resolve(checksumSha512FileName);
            if (Files.exists(checksumSha512CachePath)) {
                flag = Files.deleteIfExists(checksumSha512CachePath);
            }
            String metadataFileName = "." + FilenameUtils.getName(fileName) + ".metadata";
            Path metadataPath = artifactCachePath.getParent().resolve(metadataFileName);
            if (Files.exists(metadataPath)) {
                flag = Files.deleteIfExists(metadataPath);
            }
        } catch (Exception ex) {
            log.error("删除制品缓存checksum文件 [{}] 失败：[{}]", artifactCachePath.toString(), ExceptionUtils.getStackTrace(ex));
            flag = false;
        }
        return flag;
    }

    private void deleteCacheFile(String cachePath) {
        try {
            Path artifactCachePath = Path.of(cachePath);
            boolean flag = Files.deleteIfExists(artifactCachePath);
            if (flag) {
                //删除缓存checksum文件
                handlerArtifactCacheDelete(artifactCachePath);
            }
        } catch (Exception ex) {
            log.warn(ExceptionUtils.getStackTrace(ex));
        }
    }

    private String getHostname() {
        return IpUtils.getHostname();
    }

    /**
     * 批量删除构建物缓存记录
     * 本方法通过接收一个构建物缓存记录列表，对其进行批量删除操作
     * 采用Flux进行响应式编程，以提高处理效率和弹性
     *
     * @param records 待删除的构建物缓存记录列表如果列表为空或为null，则方法不执行任何操作
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void batchDeleteArtifactCacheRecord(List<ArtifactCacheRecord> records) {
        // 检查记录列表是否为空，如果为空则直接返回，不执行任何操作
        if (CollectionUtils.isEmpty(records)) {
            return;
        }
        // 使用Flux从records列表创建一个响应式流
        // 过滤出需要处理的记录（通过handlerDeleteArtifacte方法决定是否处理某个记录）
        // 每次缓冲100个元素，以批处理方式提高效率
        // 在弹性线程池调度器上发布，以异步方式处理数据
        // 对每个缓冲区内的记录列表，提取它们的ID，并调用artifactCacheRecordMapper的batchDelete方法进行批量删除
        // 最后，通过subscribe方法订阅这个响应式流，启动数据处理
        StopWatch stopWatch = new StopWatch();
        stopWatch.start("batchDeleteArtifactCacheRecord-0");
        Flux.fromIterable(records)
                .filter(this::handlerDeleteArtifact)
                .buffer()
                .publishOn(Schedulers.boundedElastic())
                .doOnNext(re -> {
                    StopWatch stopWatch2 = new StopWatch();
                    stopWatch2.start("batchDeleteArtifactCacheRecord-1");
                    List<Long> ids = re.stream().map(ArtifactCacheRecord::getId).collect(Collectors.toList());
                    artifactCacheRecordMapper.batchDelete(ids);
                    stopWatch2.stop();
                    log.info(stopWatch2.prettyPrint());
                })
                .subscribe();
        stopWatch.stop();
        log.info(stopWatch.prettyPrint());
    }

    /**
     * 删除构建物缓存记录
     * 本方法接收一个构建物缓存记录，并删除该记录对应的缓存文件
     * 如果缓存文件不存在，则尝试删除对应的checksum文件
     *
     * @param record 待删除的构建物缓存记录
     * @return 如果删除成功，则返回true，否则返回false
     */
    public boolean handlerDeleteArtifact(ArtifactCacheRecord record) {
        boolean flag = false;
        Path artifactCachePath = Path.of(record.getCachePath());
        try {
            if (!Files.exists(artifactCachePath)) {
                // 缓存制品文件不存在，删除缓存checksum文件
                flag = handlerArtifactCacheDelete(artifactCachePath);
                //log.info("File does not exist, attempting to delete checksum file: {}", artifactCachePath);
            } else {
                flag = deleteFileAndLog(artifactCachePath);
            }
        } catch (NoSuchFileException e) {
            log.info("File not found, which is considered a successful operation: {}", artifactCachePath);
            flag = true;
        } catch (Exception e) {
            log.error("An unexpected error occurred: ", e);
            // 不需要修改flag，保持为false表示失败
        }
        return flag;
    }

    /**
     * 删除缓存文件
     * 本方法接收一个Path对象，表示要删除的文件路径
     * 如果文件存在，则尝试删除文件，并记录删除操作
     * 如果文件不存在，则记录删除操作
     *
     * @param path 要删除的文件路径
     * @return 如果删除成功，则返回true，否则返回false
     * @throws IOException 如果删除过程中发生IO异常，则抛出IOException
     */
    private boolean deleteFileAndLog(Path path) throws IOException {
        boolean deleted = Files.deleteIfExists(path);
        if (deleted) {
            log.info("File successfully deleted: {}", path);
            // 删除缓存checksum文件
            handlerArtifactCacheDelete(path);
        } else {
            log.error("Failed to delete file: {}", path);
        }
        return deleted;
    }
}
