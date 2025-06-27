package com.veadan.folib.services;

import com.veadan.folib.storage.repository.Repository;

/**
 * @author veadan
 */
public interface MavenIndexerService {

    /**
     * 保存MavenIndexer文件
     *
     * @param format  格式
     * @param indexId 仓库名称
     * @param chainId chainId
     * @param url     仓库地址
     * @return MavenIndexer文件存储路径
     */
    String storeMavenIndexer(String format, String indexId, String chainId, String url);

    /**
     * 解析mavenIndexer文件并下载制品
     *
     * @param username         操作者
     * @param repository       仓库
     * @param mavenIndexerPath MavenIndexer文件
     * @param batch            每批数量
     * @param poolSize         设置线程池核心数量及最大数量
     */
    void handlerMavenIndexerAndDownLoad(String username, Repository repository, String mavenIndexerPath, Integer batch, Integer poolSize);
}
