package com.veadan.folib.scanner.service;


import cn.hutool.core.io.FileUtil;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONException;
import com.alibaba.fastjson.JSONObject;
import com.beust.jcommander.internal.Sets;
import com.veadan.folib.cloud.storage.s3fs.S3Path;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.VulnerabilityEntity;
import com.veadan.folib.entity.Dict;
import com.veadan.folib.enums.DictTypeEnum;
import com.veadan.folib.enums.SafeLevelEnum;
import com.veadan.folib.enums.VulnerabilityPlatformEnum;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.layout.DockerFileSystem;
import com.veadan.folib.scanner.common.exception.BusinessException;
import com.veadan.folib.scanner.common.util.DateUtils;
import com.veadan.folib.scanner.config.ScanConfig;
import com.veadan.folib.scanner.entity.ScannerReport;
import com.veadan.folib.scanner.enums.SeverityTypeEnum;
import com.veadan.folib.scanner.mapper.ScanRulesMapper;
import com.veadan.folib.services.ArtifactService;
import com.veadan.folib.services.DictService;
import com.veadan.folib.services.VulnerabilityService;
import com.veadan.folib.util.LocalDateTimeInstance;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.compress.utils.Lists;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.owasp.dependencycheck.data.update.exception.UpdateException;
import org.owasp.dependencycheck.dependency.*;
import org.owasp.dependencycheck.utils.Settings;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import software.amazon.awssdk.services.s3.model.GetObjectRequest;

import javax.inject.Inject;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.util.*;
import java.util.stream.Collectors;


@Slf4j
@Service
public class ScanService {

    @Autowired
    private ScanConfig scanConfig;

    @Inject
    protected RepositoryPathResolver repositoryPathResolver;

    @Inject
    private VulnerabilityService vulnerabilityService;

    @Inject
    private ArtifactService artifactService;

    @Inject
    private ScanRulesMapper scanRulesMapper;

    @Inject
    @Lazy
    private DictService dictService;

    @Value("${folib.temp}")
    private String tempPath;

    private Settings getSettings() {
        Settings settings = new Settings();
        settings.setString(Settings.KEYS.DB_DRIVER_NAME, "com.mysql.cj.jdbc.Driver");
        settings.setString(Settings.KEYS.DB_CONNECTION_STRING, scanConfig.getDbUrl());
        settings.setString(Settings.KEYS.DB_USER, scanConfig.getDbUser());
        settings.setString(Settings.KEYS.DB_PASSWORD, scanConfig.getDbPass());
        settings.setBoolean(Settings.KEYS.AUTO_UPDATE, false);
        settings.setBoolean(Settings.KEYS.PRETTY_PRINT, true);
        settings.setBoolean(Settings.KEYS.ANALYZER_CENTRAL_ENABLED, false);
        settings.setBoolean(Settings.KEYS.ANALYZER_NEXUS_ENABLED, false);
        settings.setBoolean(Settings.KEYS.ANALYZER_OSSINDEX_ENABLED, false);
        settings.setBoolean(Settings.KEYS.ANALYZER_RETIREJS_ENABLED, false);
        settings.setString(Settings.KEYS.ANALYZER_RETIREJS_REPO_JS_URL, "http://" + scanConfig.getScanPoxy() + "/jsrepository.json");
        settings.setString(Settings.KEYS.CVE_BASE_JSON, "http://" + scanConfig.getScanPoxy() + "/nvdcve-1.1-%d.json.gz");
        settings.setString(Settings.KEYS.CVE_MODIFIED_JSON, "http://" + scanConfig.getScanPoxy() + "/nvdcve-1.1-modified.json.gz");
        return settings;
    }

    @Async("asyncThreadPoolTaskExecutor")
    public void asyncScan(Artifact artifact) {
        try {
            //将数据库中该记录变为扫描中
            artifact.setSafeLevel(SafeLevelEnum.SCANNING.getLevel());
            artifactService.saveOrUpdateArtifact(artifact);
            Set<String> filePaths = artifact.getFilePaths();
            Set<String> filePathSet = Sets.newLinkedHashSet();
            List<Dependency> dependencyList = Lists.newArrayList(), itemDependencyList;
            for (String filePath : filePaths) {
                //执行扫描
                itemDependencyList = Arrays.asList(scanWorker(artifact, filePath));
                ScannerReport scannerReport = resolveReport(itemDependencyList);
                scannerReport.setFilePath(filePath);
                filePathSet.add(JSONObject.toJSONString(scannerReport));
                dependencyList.addAll(itemDependencyList);
            }
            artifact.setFilePaths(filePathSet);
            buildReport(artifact, dependencyList);
        } catch (Exception e) {
            artifact.setSafeLevel(SafeLevelEnum.SCAN_FAIL.getLevel());
            artifactService.saveOrUpdateArtifact(artifact);
            log.error("=====>>>>>执行扫描失败：{}", ExceptionUtils.getStackTrace(e));
            throw new BusinessException("文件解析失败");
        }
    }

    public Dependency[] scanWorker(Artifact artifact, String filePath) {
        String parentPath = null;
        XpEngine engine = null;
        try {
            engine = new XpEngine(getSettings());
            RepositoryPath repositoryPath = resolvePath(artifact);
            if (repositoryPath.getTarget() instanceof S3Path) {
                S3Path s3RepositoryPath = (S3Path) repositoryPath.getTarget();
                parentPath = tempPath + File.separator + UUID.randomUUID();
                InputStream inputStream = null;
                //s3存储
                if (repositoryPath.getFileSystem() instanceof DockerFileSystem) {
                    String temp = filePath.substring(filePath.indexOf(repositoryPath.getStorageId()));
                    S3Path s3Path = new S3Path(s3RepositoryPath.getFileSystem(), temp);
                    filePath = parentPath + File.separator + s3Path.getFileName();
                    inputStream = s3Path.getFileSystem().getClient().getObject(GetObjectRequest.builder().bucket(s3RepositoryPath.getBucketName())
                            .key(s3Path.getKey()).build());
                } else {
                    filePath = parentPath + File.separator + s3RepositoryPath.getFileName();
                    inputStream = Files.newInputStream(repositoryPath);
                }
                File tempFile = new File(filePath);
                FileUtil.writeFromStream(inputStream, tempFile, true);
            }
            log.info("=====>>>>> 扫描路径：{}", filePath);
            engine.scan(filePath);
            engine.analyzeDependencies();
            return engine.getDependencies();
        } catch (Exception ex) {
            log.error("=====>>>>>scanWorker error：{}", ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException(ex);
        } finally {
            //删除临时文件
            if (Objects.nonNull(parentPath)) {
                FileUtil.del(new File(parentPath));
            }
            if (Objects.nonNull(engine)) {
                engine.getSettings().cleanup(true);
                engine.close();
            }
        }
    }

    private ScannerReport resolveReport(List<Dependency> dependencyList) {
        ScannerReport scannerReport = ScannerReport.builder().build();
        Integer vulnerabilitiesCount = 0, dependencyCount = dependencyList.size(), dependencyVulnerabilitiesCount = 0, suppressedVulnerabilitiesCount = 0;
        for (Dependency dependency : dependencyList) {
            if (dependency.getVulnerabilities().size() > 0) {
                dependencyVulnerabilitiesCount = dependencyVulnerabilitiesCount + 1;
                vulnerabilitiesCount = vulnerabilitiesCount + dependency.getVulnerabilities().size();
            }
            if (dependency.getSuppressedVulnerabilities().size() > 0) {
                suppressedVulnerabilitiesCount = suppressedVulnerabilitiesCount + dependency.getSuppressedVulnerabilities().size();
            }
        }
        scannerReport.setDependencyCount(dependencyCount);
        scannerReport.setVulnerabilitiesCount(vulnerabilitiesCount);
        scannerReport.setDependencyVulnerabilitiesCount(dependencyVulnerabilitiesCount);
        scannerReport.setSuppressedVulnerabilitiesCount(suppressedVulnerabilitiesCount);
        Date now = new Date();
        scannerReport.setScanDate(DateUtils.getTodayDate());
        scannerReport.setScanDateTime(DateUtils.formatTime(now));
        return scannerReport;
    }

    private void buildReport(Artifact artifact, List<Dependency> dependencyList) {
        int vulnCount = 0;
        int vulnSuppressedCount = 0;
        int cpeSuppressedCount = 0;
        int vulnDepCount = 0;
        List<Dependency> dependencyLists = dependencyList;
        dependencyLists.sort((a, b) -> {
            Integer count1 = 0;
            Integer count2 = 0;
            try {
                count1 = a.getVulnerabilitiesCount();
                count2 = b.getVulnerabilitiesCount();
            } catch (JSONException e) {
                log.error("=====>>>>>处理扫描报告失败：{}", ExceptionUtils.getStackTrace(e));
            }
            return count2.compareTo(count1);
        });
        artifact.setReport(JSONArray.toJSONString(dependencyLists));
        Set<Vulnerability> vulnerabilitySet = Sets.newHashSet();
        Integer evidenceQuantity = 0;
        for (Dependency dependency : dependencyList) {
            if (dependency.getVulnerabilities().size() > 0) {
                vulnDepCount = vulnDepCount + 1;
                vulnCount = vulnCount + dependency.getVulnerabilities().size();
                vulnerabilitySet.addAll(dependency.getVulnerabilities());
            }
            if (dependency.getSuppressedIdentifiers().size() > 0) {
                cpeSuppressedCount = cpeSuppressedCount + 1;
            }
            if (dependency.getSuppressedVulnerabilities().size() > 0) {
                vulnSuppressedCount = vulnSuppressedCount + dependency.getSuppressedVulnerabilities().size();
            }
            evidenceQuantity = evidenceQuantity + dependency.getEvidence().size();
        }
        artifact.setScanDate(DateUtils.getTodayDate());
        artifact.setScanDateTime(LocalDateTimeInstance.now());
        handlerVulnerability(artifact, vulnerabilitySet);
        handlerArtifact(artifact, dependencyList.size(), vulnDepCount, vulnCount, vulnSuppressedCount, evidenceQuantity, vulnerabilitySet, SafeLevelEnum.SCAN_COMPLETE);
    }

    private RepositoryPath resolvePath(Artifact artifact) throws IOException {
        String storageId = artifact.getStorageId();
        String repositoryId = artifact.getRepositoryId();
        String artifactPath = artifact.getArtifactPath();
        return repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
    }

    /**
     * 更新制品扫描数据到图数据库
     *
     * @param artifact
     * @param dependencyCount
     * @param dependencyVulnerabilitiesCount
     * @param vulnerabilitiesCount
     * @param suppressedCount
     * @param evidenceQuantity
     * @param vulnerabilitySet
     * @param safeLevelEnum
     */
    private void handlerArtifact(Artifact artifact, Integer dependencyCount, Integer dependencyVulnerabilitiesCount, Integer vulnerabilitiesCount, Integer suppressedCount, Integer evidenceQuantity, Set<Vulnerability> vulnerabilitySet, SafeLevelEnum safeLevelEnum) {
        try {
            if (Objects.nonNull(artifact)) {
                artifact.setSafeLevel(safeLevelEnum.getLevel());
                artifact.setEvidenceQuantity(evidenceQuantity);
                artifact.setDependencyCount(dependencyCount);
                artifact.setDependencyVulnerabilitiesCount(dependencyVulnerabilitiesCount);
                artifact.setVulnerabilitiesCount(vulnerabilitiesCount);
                artifact.setSuppressedVulnerabilitiesCount(suppressedCount);
                if (CollectionUtils.isNotEmpty(vulnerabilitySet)) {
                    Set<String> vulnerabilityNameSet = vulnerabilitySet.stream().map(Vulnerability::getName).collect(Collectors.toSet());
                    artifact.setVulnerabilities(vulnerabilityNameSet);
                    long critical = vulnerabilitySet.stream().filter(item -> SeverityTypeEnum.CRITICAL.getType().equals(item.getHighestSeverityText())).count();
                    artifact.setCriticalVulnerabilitiesCount((int) critical);
                    long high = vulnerabilitySet.stream().filter(item -> SeverityTypeEnum.HIGH.getType().equals(item.getHighestSeverityText())).count();
                    artifact.setHighVulnerabilitiesCount((int) high);
                    long medium = vulnerabilitySet.stream().filter(item -> SeverityTypeEnum.MEDIUM.getType().equals(item.getHighestSeverityText())).count();
                    artifact.setMediumVulnerabilitiesCount((int) medium);
                    long low = vulnerabilitySet.stream().filter(item -> SeverityTypeEnum.LOW.getType().equals(item.getHighestSeverityText())).count();
                    artifact.setLowVulnerabilitiesCount((int) low);
                } else {
                    artifact.setVulnerabilities(Collections.emptySet());
                    artifact.setCriticalVulnerabilitiesCount(0);
                    artifact.setHighVulnerabilitiesCount(0);
                    artifact.setMediumVulnerabilitiesCount(0);
                    artifact.setLowVulnerabilitiesCount(0);
                }
                artifactService.saveOrUpdateArtifact(artifact);
            }
        } catch (Exception ex) {
            log.error("=====>>>>>更新制品扫描数据到图数据库失败：{}", ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException(ex);
        }
    }

    /**
     * 更新制品扫描数据到图数据库
     *
     * @param artifact         制品信息
     * @param vulnerabilitySet 漏洞数据
     */
    private void handlerVulnerability(Artifact artifact, Set<Vulnerability> vulnerabilitySet) {
        if (CollectionUtils.isNotEmpty(vulnerabilitySet)) {
            List<com.veadan.folib.domain.Vulnerability> vulnerabilityList = Lists.newArrayList();
            Set<String> storages = Sets.newLinkedHashSet(), storagesAndRepositories = Sets.newLinkedHashSet();
            for (Vulnerability vulnerability : vulnerabilitySet) {
                VulnerabilityEntity vulnerabilityEntity = new VulnerabilityEntity();
                vulnerabilityEntity.setUuid(vulnerability.getName());
                vulnerabilityEntity.setVulnerabilityPlatformName(VulnerabilityPlatformEnum.NVD.getName());
                CvssV2 cvssV2 = vulnerability.getCvssV2();
                if (Objects.nonNull(cvssV2)) {
                    vulnerabilityEntity.setCvssV2Score(String.valueOf(cvssV2.getScore()));
                    vulnerabilityEntity.setCvssV2Severity(cvssV2.getSeverity());
                }
                CvssV3 cvssV3 = vulnerability.getCvssV3();
                if (Objects.nonNull(cvssV3)) {
                    vulnerabilityEntity.setCvssV3Score(String.valueOf(cvssV3.getBaseScore()));
                    vulnerabilityEntity.setCvssV3Severity(cvssV3.getBaseSeverity());
                }
                vulnerabilityEntity.setDescription(vulnerability.getDescription());
                vulnerabilityEntity.setHighestSeverityText(vulnerability.getHighestSeverityText());
                VulnerableSoftware vulnerableSoftware = vulnerability.getMatchedVulnerableSoftware();
                if (Objects.nonNull(vulnerableSoftware)) {
                    vulnerabilityEntity.setVersionEndExcluding(vulnerableSoftware.getVersionEndExcluding());
                }
                storages.add(artifact.getStorageId());
                vulnerabilityEntity.setStorages(storages);
                storagesAndRepositories.add(String.format("%s-%s", artifact.getStorageId(), artifact.getRepositoryId()));
                vulnerabilityEntity.setStoragesAndRepositories(storagesAndRepositories);
                vulnerabilityList.add(vulnerabilityEntity);
            }
            vulnerabilityService.saveOrUpdateVulnerabilityBatch(vulnerabilityList);
        }
    }

    @Async("asyncThreadPoolTaskExecutor")
    public void updateDB(String username) {
        Dict existsDict = dictService.selectLatestOneDict(Dict.builder().dictType(DictTypeEnum.VULNERABILITY_UPDATE.getType()).build());
        String comment = "更新中";
        if (Objects.nonNull(existsDict) && comment.equals(existsDict.getComment())) {
            return;
        }
        Dict dict = Dict.builder().dictType(DictTypeEnum.VULNERABILITY_UPDATE.getType()).dictKey(username).createTime(new Date()).comment(comment).build();
        dictService.saveDict(dict);
        try {
            Settings settings = getSettings();
            settings.setBoolean(Settings.KEYS.UPDATE_NVDCVE_ENABLED, true);
            settings.setBoolean(Settings.KEYS.AUTO_UPDATE, true);
            XpEngine engine = new XpEngine(settings);
            engine.doUpdates();
            dictService.updateDict(Dict.builder().id(dict.getId()).comment("更新完成").build());
        } catch (UpdateException e) {
            dictService.updateDict(Dict.builder().id(dict.getId()).comment("更新错误").build());
            throw new BusinessException("更新出错");
        }
    }

    public void updateMirror() {
        Settings settings = getSettings();
        settings.setBoolean(Settings.KEYS.ENABLE_BATCH_UPDATES, true);
        settings.setBoolean(Settings.KEYS.AUTO_UPDATE, true);
        XpEngine engine = new XpEngine(settings);
        try {
            engine.doUpdates();
        } catch (UpdateException e) {
            throw new BusinessException("更新出错");
        }
    }

    /**
     * 统计properties表数据量，若小于等于1，初始化漏洞数据
     *
     * @return 数据量
     */
    public int countProperties() {
        return scanRulesMapper.countProperties();
    }
}
