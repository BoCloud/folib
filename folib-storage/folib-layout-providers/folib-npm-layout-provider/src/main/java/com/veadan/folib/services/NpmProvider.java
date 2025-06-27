package com.veadan.folib.services;

import com.veadan.folib.npm.metadata.PackageFeed;
import com.veadan.folib.npm.metadata.PackageVersion;
import com.veadan.folib.storage.repository.Repository;

/**
 * @author veadan
 **/
public interface NpmProvider {

    /**
     * 注册
     */
    void register();

    /**
     * npm package version
     *
     * @param repository  仓库
     * @param packageName packageName
     * @param version     version
     * @param targetUrl   targetUrl
     * @return 结果
     */
    PackageVersion packageVersion(Repository repository, String packageName, String version, String targetUrl);

    /**
     * npm package version
     *
     * @param repository  仓库
     * @param packageName packageName
     * @param version     version
     * @param targetUrl   targetUrl
     * @return 结果
     */
    PackageVersion getLocalPackageVersion(Repository repository, String packageName, String version, String targetUrl);

    /**
     * npm package.json
     *
     * @param repository  仓库
     * @param packageName packageName
     * @param targetUrl   targetUrl
     * @return 结果
     */
    PackageFeed packageFeed(Repository repository, String packageName, String targetUrl);

    /**
     * npm package.json
     *
     * @param repository  仓库
     * @param packageName packageName
     * @param targetUrl   targetUrl
     * @return 结果
     */
    PackageFeed getLocalPackageFeed(Repository repository, String packageName, String targetUrl);

    /**
     * npm binary
     *
     * @param repository  仓库
     * @param packageName packageName
     * @param targetUrl   targetUrl
     * @return 结果
     */
    String binary(Repository repository, String packageName, String targetUrl);

    /**
     * npm binary
     *
     * @param repository  仓库
     * @param packageName packageName
     * @param targetUrl   targetUrl
     * @return 结果
     */
    String getLocalBinary(Repository repository, String packageName, String targetUrl);
}
