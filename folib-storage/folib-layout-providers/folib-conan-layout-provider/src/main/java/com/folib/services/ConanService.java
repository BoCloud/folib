/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.services;

import com.alibaba.fastjson.JSONObject;
import com.folib.domain.SearchResults;
import com.folib.storage.repository.Repository;

/**
 * @author veadan
 * @date 2024/3/25
 **/
public interface ConanService {

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
    Object revisionsSearch(Repository repository, String artifactPath, String url);

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
