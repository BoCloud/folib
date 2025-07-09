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
package com.folib.scanner.service.impl;

import cn.hutool.core.date.DatePattern;
import cn.hutool.core.date.DateUtil;
import cn.hutool.core.io.FileUtil;
import cn.hutool.json.JSONUtil;
import com.alibaba.fastjson.JSONObject;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.folib.services.*;
import com.google.common.collect.Lists;
import com.folib.components.DistributedCacheComponent;
import com.folib.components.DistributedLockComponent;
import com.folib.components.artifact.ArtifactComponent;
import com.folib.components.cron.CronComponent;
import com.folib.components.license.LicenseComponent;
import com.folib.components.sbom.SbomComponent;
import com.folib.constant.GlobalConstants;
import com.folib.job.tasks.ArtifactScanCronJob;
import com.folib.job.tasks.VulnerabilityRefreshCronJob;
import com.folib.domain.Artifact;
import com.folib.entity.Dict;
import com.folib.enums.ArtifactMetadataEnum;
import com.folib.enums.DictTypeEnum;
import com.folib.enums.SafeLevelEnum;
import com.folib.event.artifact.ArtifactEventTypeEnum;
import com.folib.eventlistener.scanner.ArtifactEventScannerListener;
import com.folib.forms.artifact.ArtifactMetadataForm;
import com.folib.forms.dict.DictForm;
import com.folib.forms.scanner.ScannerReportForm;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.repositories.ArtifactRepository;
import com.folib.repositories.ComponentRepository;
import com.folib.scanner.analyze.SbomAnalyzeServer;
import com.folib.scanner.common.exception.BusinessException;
import com.folib.scanner.config.ScanConfig;
import com.folib.scanner.entity.ScanRules;
import com.folib.scanner.mapper.ScanRulesMapper;
import com.folib.scanner.service.SbomScannerService;
import com.folib.scanner.vulnerability.NistMirrorTask;
import com.folib.util.FileSizeConvertUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.tinkerpop.gremlin.process.traversal.Order;
import org.cyclonedx.model.Bom;
import org.cyclonedx.parsers.JsonParser;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import javax.inject.Inject;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

@Slf4j
@Service
public class SbomScannerServiceImpl implements SbomScannerService {

    @Autowired
    private ScanConfig scanConfig;

    @Inject
    @Lazy
    private VulnerabilityService vulnerabilityService;

    @Inject
    private ArtifactService artifactService;

    @Inject
    @Lazy
    private ArtifactWebService artifactWebService;

    @Inject
    private ScanRulesMapper scanRulesMapper;

    @Inject
    @Lazy
    private DictService dictService;

    @Inject
    private ComponentRepository componentRepository;

    @Inject
    @Lazy
    private VulnerabilityWebService vulnerabilityWebService;

    @Inject
    private LicenseComponent licenseComponent;

    @Inject
    @Lazy
    private ArtifactComponent artifactComponent;

    @Inject
    @Lazy
    private ArtifactEventScannerListener artifactEventScannerListener;


    @Inject
    private CronComponent cronComponent;

    @Inject
    private ArtifactRepository artifactRepository;

    @Inject
    private DistributedLockComponent distributedLockComponent;

    @Inject
    private DistributedCacheComponent distributedCacheComponent;

    @Inject
    private SbomComponent sbomComponent;

    @Value("${folib.temp}")
    private String tempPath;
    @Inject
    @Lazy
    protected RepositoryPathResolver repositoryPathResolver;
    @Inject
    @Lazy
    protected ArtifactManagementService artifactManagementService;

    @Inject
    private SbomAnalyzeServer sbomAnalsisServer;

    //@Async("asyncScanThreadPoolTaskExecutor")
    public void asyncScan(Artifact artifact) {
        doScan(artifact);
    }

    //@Async("asyncScanThreadPoolTaskExecutor")
    public void asyncScan(List<Artifact> artifactList) {
        syncScan(artifactList);
    }

    public void artifactsScan() {
        List<String> safeLevels = Lists.newArrayList(SafeLevelEnum.INIT.getLevel(), SafeLevelEnum.SCANNING.getLevel(), SafeLevelEnum.SCAN_FAIL.getLevel(), SafeLevelEnum.UN_SCAN.getLevel());
        artifactsScan(safeLevels, Order.desc.name());
    }


    public void doScan(Artifact artifact) {
        try {
            if (artifact.getSizeInBytes() > 0 && !checkSize(artifact.getSizeInBytes())) {
                log.warn("Artifact size exceeds scan limit [{}]", artifact.getUuid());
                //文件大于扫描限制，放弃扫描
                artifact.setSafeLevel(SafeLevelEnum.UNWANTED_SCAN.getLevel());
                artifactService.saveOrUpdateArtifact(artifact);
                return;
            }
            //将数据库中该记录变为扫描中
            artifact.setSafeLevel(SafeLevelEnum.SCANNING.getLevel());
            artifactService.saveOrUpdateArtifact(artifact);
            Set<String> filePaths = artifact.getFilePaths();
            for (String filePath : filePaths) {
                filePath = parseFilePath(filePath);
                //执行扫描
                String data = scanWorker(artifact, filePath);
                JsonParser jsonParser = new JsonParser();
                Bom bom = jsonParser.parse(data.getBytes(StandardCharsets.UTF_8));
                bom.getMetadata().getComponent().setName(getBomId(artifact));
                sbomAnalsisServer.analyzeCycloneDx(bom, true);
            }

        } catch (Exception e) {
            artifact.setSafeLevel(SafeLevelEnum.SCAN_FAIL.getLevel());
            artifactService.saveOrUpdateArtifact(artifact);
            log.error("执行扫描失败 [{}]", ExceptionUtils.getStackTrace(e));
            handleRetryCount(artifact);
        }
    }

    public void artifactsScan(List<String> safeLevels, String order) {
        String lockName = "ScannerTask";
        long waitTime = 3L;
        log.info("Wait for the lock [{}]", lockName);
        if (distributedLockComponent.lock(lockName, waitTime)) {
            try {
                log.info("Locked for [{}]", lockName);
                List<String> storageIdAndRepositoryIdList = getScanStorageIdAndRepositoryIdList();
                List<Artifact> artifactList = artifactRepository.findMatchingBySafeLevels(storageIdAndRepositoryIdList, safeLevels, getRetryKey(), getRetryCount(), order);
                if (CollectionUtils.isNotEmpty(artifactList)) {
                    int size = 50;
                    List<List<Artifact>> lists = Lists.partition(artifactList, size);
                    for (List<Artifact> itemList : lists) {
                        asyncScan(itemList);
                    }
                }
                log.info("Scan thread name [{}] time [{}]", Thread.currentThread().getName(), DateUtil.now());
            } finally {
                distributedLockComponent.unLock(lockName, 3500L);
            }
        } else {
            log.info("LockName [{}] was not get lock", lockName);
        }
    }


    private List<String> getScanStorageIdAndRepositoryIdList() {
        List<ScanRules> scanRulesList = scanRulesMapper.selectList(Wrappers.<ScanRules>lambdaQuery().eq(ScanRules::getOnScan, true));
        if (CollectionUtils.isEmpty(scanRulesList)) {
            return null;
        }
        return scanRulesList.stream().map(item -> String.format("%s-%s", item.getStorage(), item.getRepository())).collect(Collectors.toList());
    }

    private Integer getRetryCount() {
        Integer retryCount = GlobalConstants.SCAN_RETRY_COUNT;
        String cacheKey = distributedCacheComponent.get(GlobalConstants.SCAN_RETRY_COUNT_KEY);
        if (StringUtils.isNotBlank(cacheKey)) {
            retryCount = Integer.parseInt(cacheKey);
        }
        return retryCount;
    }

    public void syncScan(List<Artifact> artifactList) {
        if (CollectionUtils.isEmpty(artifactList)) {
            return;
        }
        long startTime = System.currentTimeMillis();
        log.info("Artifact asyncScan batch size [{}] starts with [{}]", artifactList.size(), DateUtil.format(DateUtil.date(), DatePattern.NORM_DATETIME_MS_FORMATTER));
        RepositoryPath repositoryPath = null;
        for (Artifact artifact : artifactList) {
            try {
                if (SafeLevelEnum.INIT.getLevel().equals(artifact.getSafeLevel())) {
                    //扫描状态为init的制品，重新解析下看最终是否支持扫描
                    repositoryPath = repositoryPathResolver.resolve(artifact.getStorageId(), artifact.getRepositoryId(), artifact.getArtifactPath());
                    repositoryPath.setArtifact(artifact);
                    Artifact initArtifact = artifactEventScannerListener.handle(repositoryPath, ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_STORED.getType());
                    if (Objects.isNull(initArtifact) || SafeLevelEnum.UNWANTED_SCAN.getLevel().equals(initArtifact.getSafeLevel())) {
                        continue;
                    }
                }
                doScan(artifact);
            } catch (Exception ex) {
                log.error(ExceptionUtils.getStackTrace(ex));
            } finally {
            }
        }
        long endTime = System.currentTimeMillis();
        log.info("Artifact asyncScan batch size [{}] ends with [{}] take time [{}] seconds", artifactList.size(), DateUtil.format(DateUtil.date(), DatePattern.NORM_DATETIME_MS_FORMATTER), (endTime - startTime) / 1000);
    }

    /**
     * 漏洞刷新
     *
     * @param username 用户名
     * @param cron
     */
    @Async("asyncThreadPoolTaskExecutor")
    public void vulnerabilityRefreshData(String username, String cron) {
        if (StringUtils.isNotBlank(cron)) {
            String cronName = "Vulnerability refresh";
            cronComponent.configCronTask(cronName, VulnerabilityRefreshCronJob.class.getName(), cron);
        } else {
            vulnerabilityRefresh(username);
        }
    }

    public void artifactScan(String username) {
        Dict dict = Dict.builder().dictType(DictTypeEnum.ARTIFACT_FULL_SCAN.getType()).dictKey(username).createTime(new Date()).build();
        dictService.saveDict(dict);
        try {
            //触发全量制品扫描
            artifactsFullScan(LocalDateTime.now());
        } catch (Exception e) {
            log.error("Artifact scan error [{}]", ExceptionUtils.getStackTrace(e));
        }
    }

    /**
     * 刷新cron
     *
     * @param username 用户名
     * @param cron
     */
    @Override
    @Async("asyncThreadPoolTaskExecutor")
    public void artifactScan(String username, String cron) {
        if (StringUtils.isNotBlank(cron)) {
            String cronName = "Artifact full scan";
            cronComponent.configCronTask(cronName, ArtifactScanCronJob.class.getName(), cron);
        } else {
            artifactScan(username);
        }
    }

    /**
     * 全量扫描
     *
     * @param vulnerabilityRefreshTime 漏洞数据更新时间
     */
    @Override
    public void artifactsFullScan(LocalDateTime vulnerabilityRefreshTime) {
        List<String> storageIdAndRepositoryIdList = getScanStorageIdAndRepositoryIdList();
        List<String> safeLevels = Lists.newArrayList(SafeLevelEnum.INIT.getLevel(), SafeLevelEnum.SCANNING.getLevel(), SafeLevelEnum.SCAN_FAIL.getLevel(), SafeLevelEnum.UN_SCAN.getLevel(), SafeLevelEnum.SCAN_COMPLETE.getLevel());
        long totalCount = artifactRepository.findMatchingCountBySafeLevels(storageIdAndRepositoryIdList, safeLevels);
        if (totalCount <= 0) {
            return;
        }
        int batchSize = 50;
        // 计算总页数
        int totalPages = (int) Math.ceil((double) totalCount / batchSize);
        Pageable pageable;
        Page<Artifact> page;
        List<Artifact> artifactList;
        for (int currentPage = 1; currentPage <= totalPages; currentPage++) {
            try {
                log.info("Scan totalPages [{}] currentPage [{}] batchSize [{}]", totalPages, currentPage, batchSize);
                if (currentPage == 1) {
                    pageable = PageRequest.of(currentPage, batchSize).first();
                } else {
                    pageable = PageRequest.of(currentPage, batchSize).previous();
                }
                page = artifactRepository.findMatchingPageBySafeLevels(pageable, storageIdAndRepositoryIdList, safeLevels, Order.asc.name());
                if (CollectionUtils.isNotEmpty(page.getContent())) {
                    artifactList = page.getContent();
                    //过滤扫描时间为空或者扫描时间在漏洞库更新时间之前的制品
                    artifactList = artifactList.stream().filter(item -> Objects.isNull(item.getScanDateTime()) || (Objects.nonNull(vulnerabilityRefreshTime) && item.getScanDateTime().isBefore(vulnerabilityRefreshTime))).collect(Collectors.toList());
                    syncScan(artifactList);
                }
            } catch (Exception ex) {
                log.error("Scan totalPages [{}] currentPage [{}] batchSize [{}] scan error [{}]", totalPages, currentPage, batchSize, ExceptionUtils.getStackTrace(ex));
            }
        }
    }

    public void vulnerabilityRefresh(String username) {
        Dict existsDict = dictService.selectLatestOneDict(Dict.builder().dictType(DictTypeEnum.VULNERABILITY_UPDATE.getType()).build());
        String comment = "更新中";
        if (Objects.nonNull(existsDict) && comment.equals(existsDict.getComment())) {
            return;
        }
        Dict dict = Dict.builder().dictType(DictTypeEnum.VULNERABILITY_UPDATE.getType()).dictKey(username).createTime(new Date()).comment(comment).build();
        dictService.saveDict(dict);
        try {
            NistMirrorTask nistMirrorTask = new NistMirrorTask();
            nistMirrorTask.inform();
            dictService.updateDict(DictForm.builder().id(dict.getId()).comment("更新完成").build());
            log.info("漏洞数据更新完成");
        } catch (Exception e) {
            e.printStackTrace();
            dictService.updateDict(DictForm.builder().id(dict.getId()).comment("更新错误").build());
            throw new BusinessException("更新出错");
        }
    }

    public String scanWorker(Artifact artifact, String filePath) {
        RepositoryPath bomPath = getBomPath(artifact);
        if (isConcordance(artifact, bomPath)) {
            return getBomString(bomPath);
        }
        String parentPath = tempPath + File.separator + UUID.randomUUID();
        try {
            filePath = getFilePath(parentPath, artifact, filePath);
            if (StringUtils.isBlank(filePath)) {
                return null;
            }
            return sbomComponent.getBom(Path.of(filePath));
        } catch (Exception ex) {
            log.error("ScanWorker error：{}", ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException(ex);
        } finally {
            //删除临时文件
            if (new File(parentPath).exists()) {
                FileUtil.del(new File(parentPath));
            }
        }
    }

    private boolean isConcordance(Artifact artifact, RepositoryPath bomPath) {
        String sha256 = artifact.getChecksums().get("SHA-256");
        if (sha256 == null || !Files.exists(bomPath)) {
            return false;
        }
        try (InputStream inputStream = Files.newInputStream(bomPath)) {
            JsonParser jsonParser = new JsonParser();
            Bom bom = jsonParser.parse(inputStream);
            return Optional.ofNullable(bom.getMetadata())
                    .map(metadata -> Optional.ofNullable(metadata.getComponent()))
                    .map(component -> Optional.ofNullable(component.get().getVersion()))
                    .filter(version -> version.isPresent() && version.get().endsWith(sha256))
                    .isPresent();
        } catch (IOException | org.cyclonedx.exception.ParseException e) {
            log.error("isConcordance error：{}", ExceptionUtils.getStackTrace(e));
            return false;
        }
    }

    private String getBomString(RepositoryPath bomPath) {
        String bomStr = null;
        try {
            bomStr = Files.readString(bomPath);
        } catch (IOException e) {
            log.error("getBomString error：{}", ExceptionUtils.getStackTrace(e));
        }
        return bomStr;
    }


    public RepositoryPath getBomPath(Artifact artifact) {
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(artifact.getStorageId(), artifact.getRepositoryId(), artifact.getArtifactPath());
        String metadataPath = String.format("/.%s%s/bom.json", artifact.getArtifactName(), GlobalConstants.FO_LIBRARY_METADATA);
        String metadtaFilePath = repositoryPath.getPath().replace("/" + artifact.getArtifactName(), metadataPath);
        repositoryPath = repositoryPathResolver.resolve(artifact.getStorageId(), artifact.getRepositoryId(), metadtaFilePath);
        return repositoryPath;
    }

    private String parseFilePath(String filePath) {
        if (JSONUtil.isJson(filePath)) {
            ScannerReportForm scannerReportForm = JSONObject.parseObject(filePath, ScannerReportForm.class);
            filePath = scannerReportForm.getFilePath();
            if (JSONUtil.isJson(filePath)) {
                scannerReportForm = JSONObject.parseObject(filePath, ScannerReportForm.class);
                filePath = scannerReportForm.getFilePath();
            }
        }
        return filePath;
    }

    /**
     * 解析文件路径
     *
     * @param parentPath 临时文件路径
     * @param artifact   制品
     * @param filePath   文件路径
     * @return 文件路径
     */
    private String getFilePath(String parentPath, Artifact artifact, String filePath) {
        try {
            Path path = Path.of(filePath);
            if (!Files.exists(path)) {
                log.warn("File does not exist [{}]", path.toString());
                return null;
            }
            if (!checkSize(Files.size(path))) {
                log.warn("File size exceeds scan limit [{}]", path.toString());
                return null;
            }
            return filePath;
        } catch (Exception ex) {
            log.error("Get filePath [{}] [{}] error [{}]", artifact.getUuid(), filePath, ExceptionUtils.getStackTrace(ex));
        }
        return null;
    }

    /**
     * 解析路径
     *
     * @param artifact 制品
     * @return RepositoryPath
     * @throws IOException
     */
    private RepositoryPath resolvePath(Artifact artifact) throws IOException {
        String storageId = artifact.getStorageId();
        String repositoryId = artifact.getRepositoryId();
        String artifactPath = artifact.getArtifactPath();
        return repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
    }

    /**
     * 检查文件大小
     *
     * @param sizeInBytes 文件大小
     * @return 是否超出限制
     */
    private boolean checkSize(long sizeInBytes) {
        Integer maxSize = GlobalConstants.SCAN_MAX_SIZE;
        String cacheKey = distributedCacheComponent.get(GlobalConstants.SCAN_MAX_SIZE_KEY);
        if (StringUtils.isNotBlank(cacheKey)) {
            maxSize = Integer.parseInt(cacheKey);
        }
        BigDecimal convertSize = FileSizeConvertUtils.convertBytesWithDecimal(sizeInBytes, "GB");
        if (convertSize.compareTo(new BigDecimal(maxSize)) > 0) {
            return false;
        }
        return true;
    }

    /**
     * 处理重试次数
     *
     * @param artifact 制品
     */
    private void handleRetryCount(Artifact artifact) {
        try {
            String metadata = artifact.getMetadata();
            String retryKey = getRetryKey();
            int retryCount = 0;
            boolean save = true;
            ArtifactMetadataForm artifactMetadata = ArtifactMetadataForm.builder().type(ArtifactMetadataEnum.NUMERICAL.toString()).viewShow(0).storageId(artifact.getStorageId()).repositoryId(artifact.getRepositoryId()).artifactPath(artifact.getArtifactPath()).key(retryKey).value(Integer.toString(retryCount)).build();
            if (StringUtils.isNotBlank(metadata) && JSONUtil.isJson(metadata) && JSONObject.parseObject(metadata).containsKey(retryKey)) {
                Object obj = JSONObject.parseObject(metadata).getJSONObject(retryKey).getInteger("value");
                if (Objects.nonNull(obj) && StringUtils.isNumeric(obj.toString())) {
                    retryCount = Integer.parseInt(obj.toString()) + 1;
                    artifactMetadata.setValue(Integer.toString(retryCount));
                    save = false;
                }
            }
            if (save) {
                artifactWebService.saveArtifactMetadata(artifactMetadata);
            } else {
                artifactWebService.updateArtifactMetadata(artifactMetadata);
            }
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
    }

    /**
     * 获取重试次数key
     *
     * @return key
     */
    private String getRetryKey() {
        String retryKey = GlobalConstants.SCAN_RETRY;
        String cacheKey = distributedCacheComponent.get(GlobalConstants.SCAN_RETRY_KEY);
        if (StringUtils.isNotBlank(cacheKey)) {
            retryKey = cacheKey;
        }
        return retryKey;
    }

    /**
     * 验证仓库扫描
     *
     * @param storageId    存储库id
     * @param repositoryId 仓库id
     */
    @Override
    public boolean validateRepositoryScan(String storageId, String repositoryId) {
        List<ScanRules> scanRulesList = scanRulesMapper.selectList(Wrappers.<ScanRules>lambdaQuery()
                .eq(ScanRules::getId,  String.format("%s-%s", storageId, repositoryId))
                .eq(ScanRules::getOnScan, true));
        if (CollectionUtils.isEmpty(scanRulesList)) {
            return false;
        }
        return true;
    }

    @Override
    public int countProperties() {
        return scanRulesMapper.countProperties();
    }

    @Override
    public void updateMirror() {
        try {
            NistMirrorTask nistMirrorTask = new NistMirrorTask();
            nistMirrorTask.inform();
            log.info("漏洞数据更新完成");
        } catch (Exception ex) {
            log.error("漏洞数据更新失败:{}", ExceptionUtils.getStackTrace(ex));
        }


    }

    public String getBomId(Artifact artifact){
        return String.format("%s:%s:%s", artifact.getStorageId(), artifact.getRepositoryId(), artifact.getArtifactPath());

    }
}
