package com.veadan.folib.gremlin.service;

import com.veadan.folib.domain.Artifact;

import java.io.IOException;
import java.util.List;

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
     * 导出受漏洞影响的制品信息
     * @param vulnerabilityUuid 漏洞id
     * @param storageId 存储空间id
     * @param repositoryId 仓库id
     * @throws IOException 异常
     */
    void exportExcel(String vulnerabilityUuid,
                     String storageId,
                     String repositoryId) throws IOException;

    /**
     * 导出受漏洞影响的制品信息
     * @param vulnerabilityUuid 漏洞id
     * @param storageId 存储空间id
     * @param repositoryId 仓库id
     */
    void exportPdf(String vulnerabilityUuid,
                     String storageId,
                     String repositoryId);
}
