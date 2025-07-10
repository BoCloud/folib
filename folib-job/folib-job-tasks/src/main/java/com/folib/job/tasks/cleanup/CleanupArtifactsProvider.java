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
package com.folib.job.tasks.cleanup;

import java.util.Map;

/**
 * @author veadan
 **/
public interface CleanupArtifactsProvider {

    /**
     * 注册
     */
    void register();

    /**
     * 清理方法
     *
     * @param storageId              存储空间
     * @param repositoryId           仓库名称
     * @param path                   制品路径
     * @param storageDay             保留时间
     * @param storageCondition       保留条件
     * @param cleanupArtifactPathMap 目录级别清理设置
     * @throws Exception 异常
     */
    void cleanup(String storageId, String repositoryId, String path, String storageDay, String storageCondition, Map<String, String> cleanupArtifactPathMap) throws Exception;

    /**
     * 清理方法
     *
     * @param storageId              存储空间
     * @param repositoryId           仓库名称
     * @param path                   制品路径
     * @param storageDay             保留时间
     * @param storageCondition       保留条件
     * @param cleanupArtifactPathMap 目录级别清理设置
     * @throws Exception 异常
     */
    void cleanupV2(String storageId, String repositoryId, String path, String storageDay, String storageCondition, Map<String, String> cleanupArtifactPathMap) throws Exception;
}
