package com.veadan.folib.job.tasks.cleanup;

import java.util.Map;

/**
 * @author veadan
 **/
public interface CleanupArtifactsProvider {

    /**
     * 注册
     */
    void register();

    /**
     * 清理方法
     *
     * @param storageId              存储空间
     * @param repositoryId           仓库名称
     * @param path                   制品路径
     * @param storageDay             保留时间
     * @param storageCondition       保留条件
     * @param cleanupArtifactPathMap 目录级别清理设置
     * @throws Exception 异常
     */
    void cleanup(String storageId, String repositoryId, String path, String storageDay, String storageCondition, Map<String, String> cleanupArtifactPathMap) throws Exception;

    /**
     * 清理方法
     *
     * @param storageId              存储空间
     * @param repositoryId           仓库名称
     * @param path                   制品路径
     * @param storageDay             保留时间
     * @param storageCondition       保留条件
     * @param cleanupArtifactPathMap 目录级别清理设置
     * @throws Exception 异常
     */
    void cleanupV2(String storageId, String repositoryId, String path, String storageDay, String storageCondition, Map<String, String> cleanupArtifactPathMap) throws Exception;
}
