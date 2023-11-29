package com.veadan.folib.cron.jobs.cleanup;

/**
 * @author leipenghui
 **/
public interface CleanupArtifactsProvider {

    /**
     * 注册
     */
    void register();

    /**
     * 清理方法
     *
     * @param storageId        存储空间
     * @param repositoryId     仓库名称
     * @param path             制品路径
     * @param storageDay       保留时间
     * @param storageCondition 保留条件
     * @throws Exception 异常
     */
    void cleanup(String storageId, String repositoryId, String path, String storageDay, String storageCondition) throws Exception;
}
