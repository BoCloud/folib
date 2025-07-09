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

import com.folib.storage.repository.Repository;

/**
 * @author veadan
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
     * @param username         操作者
     * @param repository       仓库
     * @param mavenIndexerPath MavenIndexer文件
     * @param batch            每批数量
     * @param poolSize         设置线程池核心数量及最大数量
     */
    void handlerMavenIndexerAndDownLoad(String username, Repository repository, String mavenIndexerPath, Integer batch, Integer poolSize);
}
