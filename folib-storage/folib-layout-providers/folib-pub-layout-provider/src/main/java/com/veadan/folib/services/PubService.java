package com.veadan.folib.services;

import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.domain.PubPackageVersionMetadata;
import com.veadan.folib.storage.repository.Repository;

/**
 * @author veadan
 **/
public interface PubService {

    /**
     * pub inspectVersion
     *
     * @param repository  仓库
     * @param packageName packageName
     * @param version     version
     * @param targetUrl   targetUrl
     * @return 结果
     */
    PubPackageVersionMetadata inspectVersion(Repository repository, String packageName, String version, String targetUrl);

    /**
     * pub packages
     *
     * @param repository  仓库
     * @param packageName packageName
     * @param targetUrl   targetUrl
     * @return 结果
     */
    JSONObject packages(Repository repository, String packageName, String targetUrl);
}
