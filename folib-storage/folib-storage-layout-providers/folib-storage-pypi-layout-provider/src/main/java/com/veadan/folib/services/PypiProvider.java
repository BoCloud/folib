package com.veadan.folib.services;

import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.storage.repository.Repository;

/**
 * @author leipenghui
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
}
