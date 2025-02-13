package com.veadan.folib.services;

import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.domain.ConanRevisions;
import com.veadan.folib.domain.SearchResults;
import com.veadan.folib.storage.repository.Repository;

/**
 * @author leipenghui
 **/
public interface ConanProvider {

    /**
     * 注册
     */
    void register();

    /**
     * 搜索
     *
     * @param version    conan 接口版本
     * @param repository 仓库
     * @param query      关键词
     * @return 结果
     */
    SearchResults search(String version, Repository repository, String query);

    /**
     * 搜索
     *
     * @param repository   仓库
     * @param artifactPath 制品路径
     * @param url          url
     * @return 结果
     */
    JSONObject revisionsSearch(Repository repository, String artifactPath, String url);

    /**
     * conan v2 revisions revisionsPackages
     *
     * @param repository   仓库
     * @param artifactPath artifactPath
     * @param targetUrl    targetUrl
     * @return 结果
     */
    JSONObject revisions(Repository repository, String artifactPath, String targetUrl);

    /**
     * conan v2 revisions revisionsPackages
     *
     * @param repository   仓库
     * @param artifactPath artifactPath
     * @param targetUrl    targetUrl
     * @return 结果
     */
    JSONObject getLocalRevisions(Repository repository, String artifactPath, String targetUrl);

    /**
     * conan v1 downloadUrls
     *
     * @param repository 仓库
     * @param name       name
     * @param version    version
     * @param user       user
     * @param channel    channel
     * @return 结果
     */
    JSONObject downloadUrls(Repository repository, String name, String version, String user, String channel);

    /**
     * conan v1 package downloadUrls
     *
     * @param repository 仓库
     * @param name       name
     * @param version    version
     * @param user       user
     * @param channel    channel
     * @param packageId  packageId
     * @return 结果
     */
    JSONObject packageDownloadUrls(Repository repository, String name, String version, String user, String channel, String packageId);

    /**
     * conan v1 digest inspect命令使用
     *
     * @param repository 仓库
     * @param name       name
     * @param version    version
     * @param user       user
     * @param channel    channel
     * @return 结果
     */
    JSONObject digest(Repository repository, String name, String version, String user, String channel);

    /**
     * conan v1 packageDigest inspect命令使用
     *
     * @param repository 仓库
     * @param name       name
     * @param version    version
     * @param user       user
     * @param channel    channel
     * @param packageId  packageId
     * @return 结果
     */
    JSONObject packageDigest(Repository repository, String name, String version, String user, String channel, String packageId);

    /**
     * conan v1 getPackageInfo
     *
     * @param repository 仓库
     * @param name       name
     * @param version    version
     * @param user       user
     * @param channel    channel
     * @param packageId  packageId
     * @param url        url
     * @return 结果
     */
    JSONObject getPackageInfo(Repository repository, String name, String version, String user, String channel, String packageId, String url);
}
