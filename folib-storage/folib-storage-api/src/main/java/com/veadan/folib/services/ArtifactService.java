package com.veadan.folib.services;

import com.veadan.folib.domain.Artifact;
import com.veadan.folib.providers.io.RepositoryPath;

import java.io.IOException;

/**
 * @author leipenghui
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
