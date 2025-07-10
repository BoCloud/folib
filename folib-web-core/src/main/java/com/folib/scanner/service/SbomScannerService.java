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
package com.folib.scanner.service;

import com.folib.domain.Artifact;
import org.springframework.scheduling.annotation.Async;

import java.time.LocalDateTime;
import java.util.List;

public interface SbomScannerService {

    /**
     * 扫描
     * @param artifact 制品
     */
    void doScan(Artifact artifact);

    /**
     * 异步扫描
     * @param artifact 制品
     */
    @Async("asyncScanThreadPoolTaskExecutor")
    void asyncScan(Artifact artifact);

    /**
     * 批量扫描
     */
    void artifactsScan();
    /**
     * 异步批量扫描
     * @param artifactList 制品列表
     */
    @Async("asyncScanThreadPoolTaskExecutor")
    void asyncScan(List<Artifact> artifactList);

    /**
     * 同步批量扫描
     * @param artifactList 制品列表
     */
    void syncScan(List<Artifact> artifactList);

    /**
     * 漏洞刷新
     * @param username 用户名
     */
    void vulnerabilityRefreshData(String username, String cron);

    /**
     * 刷新cron
     * @param username 用户名
     */
    void artifactScan(String username, String cron);

    /**
     * 全量扫描
     * @param vulnerabilityRefreshTime 漏洞数据更新时间
     */
    void artifactsFullScan(LocalDateTime vulnerabilityRefreshTime);

    /**
     * 验证仓库扫描
     * @param storageId 存储库id
     * @param repositoryId 仓库id
     */
    boolean validateRepositoryScan(String storageId, String repositoryId);

    /**
     * 获取属性数量
     */
    int countProperties();

    /**
     * 更新漏洞
     */
    void updateMirror();

    /**
     * 漏洞刷新
     * @param username 用户名
     */
    void vulnerabilityRefresh(String username);

    /**
     * 制品扫描
     * @param username 用户名
     */
    void artifactScan(String username);
}
