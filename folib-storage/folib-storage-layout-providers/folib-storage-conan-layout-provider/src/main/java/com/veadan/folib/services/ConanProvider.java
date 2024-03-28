package com.veadan.folib.services;

import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.domain.SearchResults;
import com.veadan.folib.storage.repository.Repository;

/**
 * @author leipenghui
 **/
public interface ConanSearchProvider {

    /**
     * 注册
     */
    void register();

    /**
     * 搜索
     *
     * @param repository 仓库
     * @param query      关键词
     * @return 结果
     */
    SearchResults search(Repository repository, String query);

    /**
     * 搜索
     *
     * @param repository   仓库
     * @param artifactPath 制品路径
     * @param url          url
     * @return 结果
     */
    JSONObject revisionsSearch(Repository repository, String artifactPath, String url);
}
