package com.folib.services;

import com.folib.storage.repository.Repository;

/**
 * @author veadan
 **/
public interface PypiProvider {

    /**
     * 注册
     */
    void register();

    /**
     * pypi packages
     *
     * @param repository  仓库
     * @param packageName packageName
     * @param targetUrl   targetUrl
     * @return 结果
     */
    String packages(Repository repository, String packageName, String targetUrl);

    /**
     * pypi packages
     *
     * @param repository  仓库
     * @param packageName packageName
     * @param targetUrl   targetUrl
     * @return 结果
     */
    String getLocalPackages(Repository repository, String packageName, String targetUrl);
}
