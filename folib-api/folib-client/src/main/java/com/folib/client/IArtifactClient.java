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
package com.folib.client;

import java.io.InputStream;

/**
 * Artifact processing API.
 *
 * @author veadan
 */
public interface IArtifactClient {

    /**
     * 获取baseUrl
     *
     * @return baseUrl
     */
    String getContextBaseUrl();

    /**
     * 部署文件
     *
     * @param is       inputStream输入流
     * @param url      部署地址
     * @param fileName 文件名
     * @throws ArtifactOperationException 制品操作异常
     */
    void deployFile(InputStream is, String url, String fileName) throws ArtifactOperationException;

    /**
     * 校验path是否存在
     *
     * @param path path
     * @return true 存在 false 不存在
     */
    boolean pathExists(String path);

    /**
     * 从path中获取输入流信息
     *
     * @param path path
     * @return inputStream输入流
     */
    InputStream getResource(String path);

    /**
     * 从path中获取输入流信息
     *
     * @param path   path
     * @param offset offset
     * @return inputStream输入流
     */
    InputStream getResource(String path, long offset);

    /**
     * 部署元数据
     *
     * @param is       inputStream输入流
     * @param url      部署地址
     * @param fileName 文件名称
     * @throws ArtifactOperationException 制品操作异常
     */
    void deployMetadata(InputStream is, String url, String fileName) throws ArtifactOperationException;

    /**
     * 删除制品
     *
     * @param storageId    存储空间名称
     * @param repositoryId 仓库名称
     * @param path         路径
     * @throws ArtifactOperationException 制品操作异常
     */
    void delete(String storageId, String repositoryId, String path) throws ArtifactOperationException;

    /**
     * 删除制品
     *
     * @param storageId    存储空间名称
     * @param repositoryId 仓库名称
     * @param path         路径
     * @param force        强制删除 true 是 false 否
     * @throws ArtifactOperationException 制品操作异常
     */
    void delete(String storageId, String repositoryId, String path, boolean force) throws ArtifactOperationException;
}
