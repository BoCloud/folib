package com.veadan.folib.client;

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
