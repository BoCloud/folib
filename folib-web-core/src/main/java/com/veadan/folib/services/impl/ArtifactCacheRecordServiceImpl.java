package com.veadan.folib.services.impl;

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
import tk.mybatis.mapper.entity.Example;

import javax.inject.Inject;
import java.io.File;
import java.math.BigDecimal;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.util.Date;
import java.util.List;
import java.util.Objects;
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

    @Override
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
        //数据量大时，排序影响查询速度
//        example.setOrderByClause("latest_download_time asc, size desc");
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
}
