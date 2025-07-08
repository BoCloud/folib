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
