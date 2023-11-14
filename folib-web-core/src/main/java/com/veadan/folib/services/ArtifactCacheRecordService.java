package com.veadan.folib.services;

import com.veadan.folib.entity.ArtifactCacheRecord;
import com.veadan.folib.providers.io.RepositoryPath;

import java.math.BigDecimal;
import java.util.List;

/**
 * @author leipenghui
 **/
public interface ArtifactCacheRecordService {

    /**
     * 保存制品缓存记录
     *
     * @param artifactCacheRecord 参数
     */
    void saveArtifactCacheRecord(ArtifactCacheRecord artifactCacheRecord);

    /**
     * 更新制品缓存记录
     *
     * @param artifactCacheRecord 参数
     */
    void updateArtifactCacheRecord(ArtifactCacheRecord artifactCacheRecord);

    /**
     * 删除制品缓存记录
     *
     * @param artifactCacheRecord 参数
     */
    void deleteArtifactCacheRecord(ArtifactCacheRecord artifactCacheRecord);

    /**
     * 新增或者更新制品缓存记录
     *
     * @param artifactCacheRecord 参数
     */
    void saveOrUpdateArtifactCacheRecord(ArtifactCacheRecord artifactCacheRecord);

    /**
     * 制品缓存记录
     *
     * @param artifactCacheRecord 参数
     * @return 制品缓存记录
     */
    ArtifactCacheRecord selectOneArtifactCacheRecord(ArtifactCacheRecord artifactCacheRecord);

    /**
     * 获取制品缓存记录
     *
     * @param artifactCacheRecord 参数
     * @param page                页码
     * @param limit               数量
     * @return 制品缓存记录
     */
    List<ArtifactCacheRecord> getArtifactCacheRecord(ArtifactCacheRecord artifactCacheRecord, Integer page, Integer limit);

    /**
     * 获取制品缓存记录数量
     *
     * @param artifactCacheRecord 参数
     * @return 制品缓存记录数量
     */
    int getArtifactCacheRecordCount(ArtifactCacheRecord artifactCacheRecord);

    /**
     * 删除制品缓存记录
     *
     * @param idList 参数
     */
    void deleteArtifactCacheRecordByIds(List<Long> idList);

    /**
     * 清空缓存目录
     *
     * @param directoryPath 缓存目录
     * @throws Exception 异常
     */
    void cleanupArtifactCacheDirectory(String directoryPath) throws Exception;

    /**
     * 获取缓存目录已使用大小
     *
     * @param directoryPath 缓存目录
     * @param unit          单位
     * @return 已使用大小
     * @throws Exception 异常
     */
    BigDecimal artifactCacheDirectoryUseSize(String directoryPath, String unit) throws Exception;

    /**
     * 校验制品
     * @param repositoryPath 制品
     */
    void verifySourceRepositoryPath(RepositoryPath repositoryPath);
}
