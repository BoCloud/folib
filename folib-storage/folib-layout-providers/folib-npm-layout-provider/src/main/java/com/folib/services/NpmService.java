package com.folib.services;

import com.folib.npm.metadata.PackageFeed;
import com.folib.npm.metadata.PackageVersion;
import com.folib.storage.repository.Repository;

/**
 * @author veadan
 **/
public interface NpmService {

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
     * npm package.json
     *
     * @param repository  仓库
     * @param packageName packageName
     * @param targetUrl   targetUrl
     * @return 结果
     */
    PackageFeed packageFeed(Repository repository, String packageName, String targetUrl);

    /**
     * npm binary
     *
     * @param repository  仓库
     * @param packageName packageName
     * @param targetUrl   targetUrl
     * @return 结果
     */
    String binary(Repository repository, String packageName, String targetUrl);
}
