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

import com.folib.domain.Artifact;
import com.folib.providers.io.RepositoryPath;

import java.io.IOException;

/**
 * @author veadan
 * @date 2022/10/8
 **/
public interface ArtifactService {

    /**
     * 保存或者更新制品顶点
     *
     * @param artifact 制品
     */
    void saveOrUpdateArtifact(Artifact artifact);

    /**
     * 保存或者更新制品顶点
     *
     * @param artifact    制品
     * @param immediately 立即保存
     */
    void saveOrUpdateArtifact(Artifact artifact, Boolean immediately);

    /**
     * 查找制品和扫描报告
     *
     * @param repositoryPath 制品路径
     * @param report         true 返回安全报告 false 不返回安全报告
     * @return 制品
     * @throws IOException io异常
     */
    Artifact findArtifact(RepositoryPath repositoryPath, Boolean report) throws IOException;

    /**
     * 保存或者更新制品顶点
     *
     * @param sourceRepositoryPath 源制品
     * @param targetRepositoryPath 目标制品
     * @throws IOException io异常
     */
    void copyArtifact(RepositoryPath sourceRepositoryPath, RepositoryPath targetRepositoryPath) throws IOException;

    /**
     * 获取制品信息
     *
     * @param repositoryPath 制品信息
     * @return Artifact
     * @throws IOException 异常
     */
    Artifact provideArtifact(RepositoryPath repositoryPath) throws IOException;
}
