package com.veadan.folib.services;

import com.veadan.folib.storage.repository.Repository;

/**
 * @author leipenghui
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
     * @param repository       仓库
     * @param mavenIndexerPath MavenIndexer文件
     * @param batch            每批数量
     */
    void handlerMavenIndexerAndDownLoad(Repository repository, String mavenIndexerPath, Integer batch);
}
