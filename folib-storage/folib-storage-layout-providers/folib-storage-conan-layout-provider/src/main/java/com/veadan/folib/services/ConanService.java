package com.veadan.folib.services;

import com.veadan.folib.domain.SearchResults;
import com.veadan.folib.storage.repository.Repository;

/**
 * @author leipenghui
 * @date 2024/3/25
 **/
public interface ConanService {

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
    Object revisionsSearch(Repository repository, String artifactPath, String url);
}
