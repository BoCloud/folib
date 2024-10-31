package com.veadan.folib.scanner.service;


import cn.hutool.core.date.DatePattern;
import cn.hutool.core.date.DateUtil;
import cn.hutool.core.io.FileUtil;
import cn.hutool.json.JSONUtil;
import com.alibaba.fastjson.JSONException;
import com.alibaba.fastjson.JSONObject;
import com.beust.jcommander.internal.Sets;
import com.google.common.collect.Lists;
import com.veadan.folib.cloud.storage.s3fs.S3Path;
import com.veadan.folib.cluster.SyncCornJobEnum;
import com.veadan.folib.components.DistributedCacheComponent;
import com.veadan.folib.components.DistributedLockComponent;
import com.veadan.folib.components.artifact.ArtifactComponent;
import com.veadan.folib.components.license.LicenseComponent;
import com.veadan.folib.components.sbom.BomComponent;
import com.veadan.folib.components.sbom.SbomComponent;
import com.veadan.folib.components.scan.ScanComponent;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.controllers.cluster.dto.SyncCronJobDto;
import com.veadan.folib.cron.domain.CronTaskConfigurationDto;
import com.veadan.folib.cron.jobs.ArtifactScanCronJob;
import com.veadan.folib.cron.jobs.VulnerabilityRefreshCronJob;
import com.veadan.folib.cron.services.CronTaskConfigurationService;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.Component;
import com.veadan.folib.domain.ComponentEntity;
import com.veadan.folib.domain.VulnerabilityEntity;
import com.veadan.folib.entity.Dict;
import com.veadan.folib.entity.License;
import com.veadan.folib.enums.ArtifactMetadataEnum;
import com.veadan.folib.enums.DictTypeEnum;
import com.veadan.folib.enums.SafeLevelEnum;
import com.veadan.folib.enums.VulnerabilityPlatformEnum;
import com.veadan.folib.event.artifact.ArtifactEventTypeEnum;
import com.veadan.folib.eventlistener.scanner.ArtifactEventScannerListener;
import com.veadan.folib.forms.artifact.ArtifactMetadataForm;
import com.veadan.folib.forms.dict.DictForm;
import com.veadan.folib.forms.scanner.ScannerReportForm;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.layout.DockerFileSystem;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.repositories.ComponentRepository;
import com.veadan.folib.scanner.common.exception.BusinessException;
import com.veadan.folib.scanner.common.util.DateUtils;
import com.veadan.folib.scanner.config.ScanConfig;
import com.veadan.folib.scanner.entity.ScanRules;
import com.veadan.folib.scanner.entity.ScannerReport;
import com.veadan.folib.scanner.enums.SeverityTypeEnum;
import com.veadan.folib.scanner.mapper.ScanRulesMapper;
import com.veadan.folib.services.*;
import com.veadan.folib.util.FileSizeConvertUtils;
import com.veadan.folib.util.LocalDateTimeInstance;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.commons.text.similarity.JaccardSimilarity;
import org.apache.commons.text.similarity.LevenshteinDistance;
import org.apache.tinkerpop.gremlin.process.traversal.Order;
import org.owasp.dependencycheck.data.update.exception.UpdateException;
import org.owasp.dependencycheck.dependency.*;
import org.owasp.dependencycheck.dependency.naming.Identifier;
import org.owasp.dependencycheck.dependency.naming.PurlIdentifier;
import org.owasp.dependencycheck.utils.Checksum;
import org.owasp.dependencycheck.utils.Settings;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import tk.mybatis.mapper.entity.Example;

import javax.inject.Inject;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDateTime;
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
    private ScanComponent scanComponent;

    @Inject
    private CronTaskConfigurationService cronTaskConfigurationService;

    @Inject
    private ClusterSyncService clusterSyncService;

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

    private Settings getSettings() {
        Settings settings = new Settings();
        settings.setString(Settings.KEYS.DB_DRIVER_NAME, scanConfig.getDriverClassName());
        settings.setString(Settings.KEYS.DB_CONNECTION_STRING, scanConfig.getDbUrl());
        settings.setString(Settings.KEYS.DB_USER, scanConfig.getDbUser());
        settings.setString(Settings.KEYS.DB_PASSWORD, scanConfig.getDbPass());
        settings.setBoolean(Settings.KEYS.AUTO_UPDATE, false);
        settings.setBoolean(Settings.KEYS.PRETTY_PRINT, true);
        settings.setBoolean(Settings.KEYS.ANALYZER_CENTRAL_ENABLED, false);
        settings.setBoolean(Settings.KEYS.ANALYZER_NEXUS_ENABLED, false);
        settings.setBoolean(Settings.KEYS.ANALYZER_OSSINDEX_ENABLED, false);
        settings.setBoolean(Settings.KEYS.ANALYZER_RETIREJS_ENABLED, false);
        settings.setBoolean(Settings.KEYS.ANALYZER_EXPERIMENTAL_ENABLED, true);
        settings.setString(Settings.KEYS.ANALYZER_RETIREJS_REPO_JS_URL, "http://" + scanConfig.getScanPoxy() + "/jsrepository.json");
        settings.setString(Settings.KEYS.CVE_BASE_JSON, "http://" + scanConfig.getScanPoxy() + "/nvdcve-1.1-%d.json.gz");
        settings.setString(Settings.KEYS.CVE_MODIFIED_JSON, "http://" + scanConfig.getScanPoxy() + "/nvdcve-1.1-modified.json.gz");
        return settings;
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
            Set<String> filePathSet = Sets.newLinkedHashSet();
            List<Dependency> dependencyList = Lists.newArrayList(), itemDependencyList;
            List<BomComponent> sbomComponentList = Lists.newArrayList();
            Dependency[] dependencies = null;
            for (String filePath : filePaths) {
                filePath = parseFilePath(filePath);
                //执行扫描
                dependencies = scanWorker(artifact, filePath, sbomComponentList);
                if (Objects.isNull(dependencies)) {
                    continue;
                }
                itemDependencyList = Arrays.asList(dependencies);
                ScannerReport scannerReport = resolveReport(itemDependencyList);
                scannerReport.setFilePath(filePath);
                filePathSet.add(JSONObject.toJSONString(scannerReport));
                dependencyList.addAll(itemDependencyList);
                itemDependencyList = null;
            }
            artifact.setFilePaths(filePathSet);
            List<License> licenses = licenseComponent.getLicenses();
            buildReport(licenses, artifact, dependencyList, sbomComponentList);
            dependencyList.clear();
            dependencyList = null;
        } catch (Exception e) {
            artifact.setSafeLevel(SafeLevelEnum.SCAN_FAIL.getLevel());
            artifactService.saveOrUpdateArtifact(artifact);
            log.error("执行扫描失败 [{}]", ExceptionUtils.getStackTrace(e));
            handleRetryCount(artifact);
        }
        artifact.setReport("");
    }

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

    @Async("asyncScanThreadPoolTaskExecutor")
    public void asyncScan(Artifact artifact) {
        doScan(artifact);
    }

    @Async("asyncScanThreadPoolTaskExecutor")
    public void asyncScan(List<Artifact> artifactList) {
        syncScan(artifactList);
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
                Checksum.clearCache();
            }
        }
        long endTime = System.currentTimeMillis();
        log.info("Artifact asyncScan batch size [{}] ends with [{}] take time [{}] seconds", artifactList.size(), DateUtil.format(DateUtil.date(), DatePattern.NORM_DATETIME_MS_FORMATTER), (endTime - startTime) / 1000);
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

    private String getFilePath(String parentPath, Artifact artifact, String filePath) {
        try {
            RepositoryPath repositoryPath = resolvePath(artifact);
            if (repositoryPath.getTarget() instanceof S3Path) {
                Path artifactPath;
                S3Path s3RepositoryPath = (S3Path) repositoryPath.getTarget();
                //s3存储
                if (repositoryPath.getFileSystem() instanceof DockerFileSystem) {
                    String temp = filePath.substring(filePath.indexOf(repositoryPath.getStorageId()));
                    if (!temp.startsWith(File.separator)) {
                        temp = File.separator + temp;
                    }
                    S3Path s3Path = new S3Path(s3RepositoryPath.getFileSystem(), temp);
                    filePath = parentPath + File.separator + s3Path.getFileName();
                    artifactPath = s3Path;
                } else {
                    filePath = parentPath + File.separator + s3RepositoryPath.getFileName();
                    artifactPath = repositoryPath;
                }
                File tempFile = new File(filePath);
                try (InputStream inputStream = Files.newInputStream(artifactPath)) {
                    FileUtil.writeFromStream(inputStream, tempFile);
                }
            }
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

    public Dependency[] scanWorker(Artifact artifact, String filePath, List<BomComponent> sbomComponentList) {
        String parentPath = tempPath + File.separator + UUID.randomUUID();
        XpEngine engine = null;
        try {
            engine = new XpEngine(getSettings());
            filePath = getFilePath(parentPath, artifact, filePath);
            if (StringUtils.isBlank(filePath)) {
                return null;
            }
            List<BomComponent> sbomComponents = sbomComponent.sbomComponent(Path.of(filePath));
            if (CollectionUtils.isNotEmpty(sbomComponents)) {
                sbomComponentList.addAll(sbomComponents);
            }
            engine.scan(filePath);
            engine.analyzeDependencies();
            return engine.getDependencies();
        } catch (Exception ex) {
            log.error("ScanWorker error：{}", ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException(ex);
        } finally {
            if (Objects.nonNull(engine)) {
                engine.getSettings().cleanup(true);
                engine.close();
            }
            //删除临时文件
            if (new File(parentPath).exists()) {
                FileUtil.del(new File(parentPath));
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

    private void buildReport(List<License> licenses, Artifact artifact, List<Dependency> dependencyList, List<BomComponent> sbomComponentList) {
        int vulnCount = 0;
        int vulnSuppressedCount = 0;
        int cpeSuppressedCount = 0;
        int vulnDepCount = 0;
        dependencyList.sort((a, b) -> {
            Integer count1 = 0;
            Integer count2 = 0;
            try {
                count1 = a.getVulnerabilitiesCount();
                count2 = b.getVulnerabilitiesCount();
            } catch (JSONException e) {
                log.error("处理扫描报告失败：{}", ExceptionUtils.getStackTrace(e));
            }
            return count2.compareTo(count1);
        });
        artifact.setReport("[]");
        scanComponent.writeReport(artifact, dependencyList);
        Set<Vulnerability> vulnerabilitySet = Sets.newHashSet();
        int evidenceQuantity = 0;
        Set<Component> componentSet = Sets.newLinkedHashSet();
        List<String> licenseIds = licenses.stream().map(License::getLicenseId).collect(Collectors.toList());
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
            buildComponent(licenses, licenseIds, dependency, componentSet, sbomComponentList);
        }
        artifact.setScanDate(DateUtils.getTodayDate());
        artifact.setScanDateTime(LocalDateTimeInstance.now());
        handlerVulnerability(artifact, vulnerabilitySet);
        handlerComponent(componentSet);
        handlerArtifact(artifact, dependencyList.size(), vulnDepCount, vulnCount, vulnSuppressedCount, evidenceQuantity, vulnerabilitySet, SafeLevelEnum.SCAN_COMPLETE, componentSet);
    }

    /**
     * 构建组件
     *
     * @param licenses          license列表
     * @param licenseIds        licenseId列表
     * @param dependency        dependency
     * @param componentSet      componentSet
     * @param sbomComponentList sbom的扫描结果
     */
    private void buildComponent(List<License> licenses, List<String> licenseIds, Dependency dependency, Set<Component> componentSet, List<BomComponent> sbomComponentList) {
        String nameKey = "name", groupIdKey = "groupId", versionKey = "version", fileName;
        LocalDateTime now = LocalDateTimeInstance.now();
        Component component = new ComponentEntity(dependency.getSha1sum());
        component.setCreated(now);
        component.setLastUpdated(now);
        if (StringUtils.isNotBlank(dependency.getFileName())) {
            fileName = dependency.getFileName();
            if (fileName.contains(": ")) {
                fileName = fileName.substring(fileName.indexOf(": ")).replace(": ", "");
            }
            component.setFileName(fileName);
        }
        component.setDescription(dependency.getDescription());
        component.setMd5sum(dependency.getMd5sum());
        component.setSha256sum(dependency.getSha256sum());
        if (CollectionUtils.isNotEmpty(licenses)) {
            Set<String> licenseSet = Sets.newLinkedHashSet();
            if (StringUtils.isNotBlank(dependency.getLicense())) {
                log.debug("Dependency license [{}]", dependency.getLicense());
                String[] dependencyLicenses = dependency.getLicense().split(",");
                LevenshteinDistance levenshteinDistance = new LevenshteinDistance();
                JaccardSimilarity jaccardSimilarity = new JaccardSimilarity();
                double levenshteinSimilarValue = getLevenshteinDistanceSimilarValue();
                double jaccardSimilarValue = getJaccardSimilarValue();
                for (String dependencyLicense : dependencyLicenses) {
                    if (StringUtils.isNotBlank(dependencyLicense)) {
                        for (License license : licenses) {
                            if (StringUtils.isNotBlank(license.getLicenseUrl())) {
                                if (Arrays.stream(license.getLicenseUrl().split(","))
                                        .anyMatch(licenseUrl -> {
                                            double licenseUrlLevenshteinResult = levenshteinDistance.apply(dependencyLicense, licenseUrl);
                                            double licenseUrlJaccardResult = jaccardSimilarity.apply(dependencyLicense, licenseUrl);
                                            log.info("License [{}] dependencyLicense [{}] licenseUrl [{}] levenshteinSimilarValue [{}] jaccardSimilarValue [{}] licenseUrlLevenshteinResult [{}] licenseUrlJaccardResult [{}]", license.getLicenseId(), dependencyLicense, licenseUrl, levenshteinSimilarValue, jaccardSimilarValue, licenseUrlLevenshteinResult, licenseUrlJaccardResult);
                                            boolean similar = (licenseUrlLevenshteinResult <= levenshteinSimilarValue && licenseUrlJaccardResult >= jaccardSimilarValue);
                                            if (similar) {
                                                log.info("License [{}] dependencyLicense [{}] licenseUrl [{}] levenshteinSimilarValue [{}] jaccardSimilarValue [{}] licenseUrlLevenshteinResult [{}] licenseUrlJaccardResult [{}] similar value matching", license.getLicenseId(), dependencyLicense, licenseUrl, levenshteinSimilarValue, jaccardSimilarValue, licenseUrlLevenshteinResult, licenseUrlJaccardResult);
                                                return true;
                                            }
                                            if (dependencyLicense.contains(licenseUrl)) {
                                                log.info("License [{}] dependencyLicense [{}] contains licenseUrl [{}]", license.getLicenseId(), dependencyLicense, licenseUrl);
                                                return true;
                                            }
                                            return false;
                                        })) {
                                    licenseSet.add(license.getLicenseId());
                                }
                            }
                            double licenseNameLevenshteinResult = levenshteinDistance.apply(dependencyLicense, license.getLicenseName());
                            double licenseNameJaccardResult = jaccardSimilarity.apply(dependencyLicense, license.getLicenseName());
                            log.info("License [{}] dependencyLicense [{}] licenseName [{}] levenshteinSimilarValue [{}] jaccardSimilarValue [{}] licenseUrlLevenshteinResult [{}] licenseUrlJaccardResult [{}]", license.getLicenseId(), dependencyLicense, license.getLicenseName(), levenshteinSimilarValue, jaccardSimilarValue, licenseNameLevenshteinResult, licenseNameJaccardResult);
                            if (licenseNameLevenshteinResult <= levenshteinSimilarValue && licenseNameJaccardResult >= jaccardSimilarValue) {
                                log.info("License [{}] dependencyLicense [{}] licenseName [{}] levenshteinSimilarValue [{}] jaccardSimilarValue [{}] licenseUrlLevenshteinResult [{}] licenseUrlJaccardResult [{}] similar value matching", license.getLicenseId(), dependencyLicense, license.getLicenseName(), levenshteinSimilarValue, jaccardSimilarValue, licenseNameLevenshteinResult, licenseNameJaccardResult);
                                licenseSet.add(license.getLicenseId());
                            }
                            if (Arrays.stream(dependencyLicense.split(": "))
                                    .anyMatch(item -> {
                                        if (StringUtils.isNotBlank(license.getLicenseUrl())) {
                                            if (Arrays.stream(license.getLicenseUrl().split(","))
                                                    .anyMatch(licenseUrl -> {
                                                        double licenseUrlLevenshteinResult = levenshteinDistance.apply(item, licenseUrl);
                                                        double licenseUrlJaccardResult = jaccardSimilarity.apply(item, licenseUrl);
                                                        log.info("License [{}] dependencyLicense [{}] item [{}] licenseUrl [{}] levenshteinSimilarValue [{}] jaccardSimilarValue [{}] licenseUrlLevenshteinResult [{}] licenseUrlJaccardResult [{}]", license.getLicenseId(), dependencyLicense, item, licenseUrl, levenshteinSimilarValue, jaccardSimilarValue, licenseUrlLevenshteinResult, licenseUrlJaccardResult);
                                                        boolean similar = (licenseUrlLevenshteinResult <= levenshteinSimilarValue && licenseUrlJaccardResult >= jaccardSimilarValue);
                                                        if (similar) {
                                                            log.info("License [{}] dependencyLicense [{}] item [{}] licenseUrl [{}] levenshteinSimilarValue [{}] jaccardSimilarValue [{}] licenseUrlLevenshteinResult [{}] licenseUrlJaccardResult [{}] similar value matching", license.getLicenseId(), dependencyLicense, item, licenseUrl, levenshteinSimilarValue, jaccardSimilarValue, licenseUrlLevenshteinResult, licenseUrlJaccardResult);
                                                            return true;
                                                        }
                                                        return false;
                                                    })) {
                                                licenseSet.add(license.getLicenseId());
                                            }
                                        }
                                        double licenseNameItemLevenshteinResult = levenshteinDistance.apply(item, license.getLicenseName());
                                        double licenseNameItemJaccardResult = jaccardSimilarity.apply(item, license.getLicenseName());
                                        log.info("License [{}] dependencyLicense [{}] item [{}] licenseName [{}] levenshteinSimilarValue [{}] jaccardSimilarValue [{}] licenseUrlLevenshteinResult [{}] licenseUrlJaccardResult [{}]", license.getLicenseId(), dependencyLicense, item, license.getLicenseName(), levenshteinSimilarValue, jaccardSimilarValue, licenseNameItemLevenshteinResult, licenseNameItemJaccardResult);
                                        if (licenseNameItemLevenshteinResult <= levenshteinSimilarValue && licenseNameItemJaccardResult >= jaccardSimilarValue) {
                                            log.info("License [{}] dependencyLicense [{}] item [{}] licenseName [{}] levenshteinSimilarValue [{}] jaccardSimilarValue [{}] licenseUrlLevenshteinResult [{}] licenseUrlJaccardResult [{}] similar value matching", license.getLicenseId(), dependencyLicense, item, license.getLicenseName(), levenshteinSimilarValue, jaccardSimilarValue, licenseNameItemLevenshteinResult, licenseNameItemJaccardResult);
                                            return true;
                                        }
                                        return false;
                                    })) {
                                licenseSet.add(license.getLicenseId());
                            }
                        }
                    }
                }
            }
            Optional<BomComponent> sbomComponentOptional = sbomComponentList.stream().filter(sbom -> dependency.getSha1sum().equalsIgnoreCase(sbom.getSha1())).findFirst();
            sbomComponentOptional.ifPresent(bomComponent -> {
                if (CollectionUtils.isNotEmpty(bomComponent.getLicenses())) {
                    bomComponent.getLicenses().forEach(license -> {
                        log.info("Component [{}] [{}] sbom license [{}]", component.getUuid(), component.getFileName(), JSONObject.toJSONString(license));
                        if (Objects.nonNull(license) && Objects.nonNull(license.getLicense()) && StringUtils.isNotBlank(license.getLicense().getId()) && licenseIds.contains(license.getLicense().getId())) {
                            licenseSet.add(license.getLicense().getId());
                        }
                    });
                }
            });
            log.debug("LicenseSet {}", licenseSet);
            component.setLicense(licenseSet);
        }
        if (CollectionUtils.isNotEmpty(dependency.getVulnerabilities())) {
            Set<Vulnerability> vulnerabilitySet = dependency.getVulnerabilities();
            component.setVulnerabilitiesCount(vulnerabilitySet.size());
            component.setVulnerabilities(vulnerabilitySet.stream().map(Vulnerability::getName).collect(Collectors.toSet()));
            long critical = vulnerabilitySet.stream().filter(item -> SeverityTypeEnum.CRITICAL.getType().equals(item.getHighestSeverityText())).count();
            component.setCriticalVulnerabilitiesCount((int) critical);
            long high = vulnerabilitySet.stream().filter(item -> SeverityTypeEnum.HIGH.getType().equals(item.getHighestSeverityText())).count();
            component.setHighVulnerabilitiesCount((int) high);
            long medium = vulnerabilitySet.stream().filter(item -> SeverityTypeEnum.MEDIUM.getType().equals(item.getHighestSeverityText())).count();
            component.setMediumVulnerabilitiesCount((int) medium);
            long low = vulnerabilitySet.stream().filter(item -> SeverityTypeEnum.LOW.getType().equals(item.getHighestSeverityText())).count();
            component.setLowVulnerabilitiesCount((int) low);
        }
        if (CollectionUtils.isNotEmpty(dependency.getSuppressedVulnerabilities())) {
            component.setSuppressedVulnerabilitiesCount(dependency.getSuppressedVulnerabilities().size());
        }
        if (CollectionUtils.isNotEmpty(dependency.getEvidence())) {
            List<Evidence> groupIdEvidenceList = dependency.getEvidence().stream().filter(evidence -> groupIdKey.equalsIgnoreCase(evidence.getName())).collect(Collectors.toList());
            if (CollectionUtils.isNotEmpty(groupIdEvidenceList)) {
                groupIdEvidenceList.sort(Comparator.comparing(Evidence::getConfidence));
                component.setGroupId(groupIdEvidenceList.get(0).getValue());
            }
            if (StringUtils.isNotBlank(dependency.getName())) {
                component.setName(dependency.getName());
            } else {
                List<Evidence> nameEvidenceList = dependency.getEvidence().stream().filter(evidence -> nameKey.equalsIgnoreCase(evidence.getName())).collect(Collectors.toList());
                if (CollectionUtils.isNotEmpty(nameEvidenceList)) {
                    nameEvidenceList.sort(Comparator.comparing(Evidence::getConfidence));
                    component.setName(nameEvidenceList.get(0).getValue());
                }
            }
            if (StringUtils.isNotBlank(dependency.getVersion())) {
                component.setVersion(dependency.getVersion());
            } else {
                List<Evidence> versionEvidenceList = dependency.getEvidence().stream().filter(evidence -> versionKey.equalsIgnoreCase(evidence.getName())).collect(Collectors.toList());
                if (CollectionUtils.isNotEmpty(versionEvidenceList)) {
                    versionEvidenceList.sort(Comparator.comparing(Evidence::getConfidence));
                    component.setVersion(versionEvidenceList.get(0).getValue());
                }
            }
            if (CollectionUtils.isNotEmpty(dependency.getSoftwareIdentifiers())) {
                List<Identifier> identifierList = Lists.newArrayList();
                identifierList.addAll(dependency.getSoftwareIdentifiers());
                identifierList.sort(Comparator.comparing(Identifier::getConfidence));
                Identifier identifier = identifierList.get(0);
                if (identifier instanceof PurlIdentifier) {
                    PurlIdentifier purlIdentifier = (PurlIdentifier) identifier;
                    if (StringUtils.isNotBlank(purlIdentifier.getNamespace())) {
                        component.setGroupId(purlIdentifier.getNamespace());
                    }
                    if (StringUtils.isNotBlank(purlIdentifier.getName())) {
                        component.setName(purlIdentifier.getName());
                    }
                    if (StringUtils.isNotBlank(purlIdentifier.getValue())) {
                        component.setPurl(purlIdentifier.getValue());
                    }
                    if (StringUtils.isNotBlank(purlIdentifier.getVersion())) {
                        component.setVersion(purlIdentifier.getVersion());
                    }
                    if (StringUtils.isNotBlank(purlIdentifier.getUrl())) {
                        component.setUrl(purlIdentifier.getUrl());
                    }
                }
            }
        }
        componentSet.add(component);
    }

    /**
     * 组件保存到图库
     *
     * @param componentSet componentSet
     */
    private void handlerComponent(Set<Component> componentSet) {
        if (CollectionUtils.isNotEmpty(componentSet)) {
            componentSet.forEach(component -> {
                componentRepository.saveOrUpdate(component);
            });
        }
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
     * @param componentSet
     */
    private void handlerArtifact(Artifact artifact, Integer dependencyCount, Integer dependencyVulnerabilitiesCount, Integer vulnerabilitiesCount, Integer suppressedCount, Integer evidenceQuantity, Set<Vulnerability> vulnerabilitySet, SafeLevelEnum safeLevelEnum, Set<Component> componentSet) {
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
                    artifact.setVulnerabilities(Collections.singleton("drop"));
                    artifact.setCriticalVulnerabilitiesCount(0);
                    artifact.setHighVulnerabilitiesCount(0);
                    artifact.setMediumVulnerabilitiesCount(0);
                    artifact.setLowVulnerabilitiesCount(0);
                }
                if (CollectionUtils.isNotEmpty(componentSet)) {
                    artifact.setComponentSet(componentSet);
                } else {
                    artifact.setComponentSet(Collections.singleton(new ComponentEntity("drop")));
                }
                artifactService.saveOrUpdateArtifact(artifact);
                RepositoryPath repositoryPath = repositoryPathResolver.resolve(artifact.getStorageId(), artifact.getRepositoryId(), artifact.getArtifactPath());
                artifactComponent.storeArtifactMetadataFile(repositoryPath);
                if (CollectionUtils.isNotEmpty(artifact.getVulnerabilitySet())) {
                    List<com.veadan.folib.domain.Vulnerability> vulnerabilityList = Lists.newArrayList();
                    vulnerabilityList.addAll(artifact.getVulnerabilitySet());
                    vulnerabilityWebService.handlerStoragesAndRepositoriesByVulnerabilityList(artifact.getStorageId(), artifact.getRepositoryId(), vulnerabilityList);
                }
            }
        } catch (Exception ex) {
            log.error("更新制品扫描数据到图数据库失败：{}", ExceptionUtils.getStackTrace(ex));
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
    public void vulnerabilityRefreshData(String username, String cron) {
        if (StringUtils.isNotBlank(cron)) {
            String cronName = "Vulnerability refresh";
            configCronTask(cronName, VulnerabilityRefreshCronJob.class.getName(), cron);
        } else {
            vulnerabilityRefresh(username);
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
            Settings settings = getSettings();
            settings.setBoolean(Settings.KEYS.UPDATE_NVDCVE_ENABLED, true);
            settings.setBoolean(Settings.KEYS.AUTO_UPDATE, true);
            XpEngine engine = new XpEngine(settings);
            boolean result = engine.doUpdates();
            if (!result) {
                dictService.updateDict(DictForm.builder().id(dict.getId()).comment("漏洞数据没有任何更新").build());
                log.info("漏洞数据实际没有进行任何更新");
            } else {
                try {
                    dictService.updateDict(DictForm.builder().id(dict.getId()).comment("更新完成").build());
                } catch (Exception ex) {
                    log.warn(ExceptionUtils.getStackTrace(ex));
                }
                log.info("漏洞数据更新完成");
            }
        } catch (UpdateException e) {
            dictService.updateDict(DictForm.builder().id(dict.getId()).comment("更新错误").build());
            throw new BusinessException("更新出错");
        }
    }

    @Async("asyncThreadPoolTaskExecutor")
    public void artifactScan(String username, String cron) {
        if (StringUtils.isNotBlank(cron)) {
            String cronName = "Artifact full scan";
            configCronTask(cronName, ArtifactScanCronJob.class.getName(), cron);
        } else {
            artifactScan(username);
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

    private void configCronTask(String cronName, String className, String cron) {
        CronTaskConfigurationDto cronTaskConfiguration = new CronTaskConfigurationDto();
        cronTaskConfiguration.setName(cronName);
        cronTaskConfiguration.setJobClass(className);
        cronTaskConfiguration.setCronExpression(cron);
        cronTaskConfiguration.setOneTimeExecution(false);
        cronTaskConfiguration.setImmediateExecution(false);
        try {
            Optional<CronTaskConfigurationDto> cronTaskConfigurationOptional = cronTaskConfigurationService.getTasksConfigurationDto().getCronTaskConfigurations().stream().filter(item -> item.getJobClass().equals(className)).findFirst();
            if (cronTaskConfigurationOptional.isPresent()) {
                CronTaskConfigurationDto cronTaskConfigurationDto = cronTaskConfigurationOptional.get();
                cronTaskConfigurationService.deleteConfiguration(cronTaskConfigurationDto.getUuid());
                SyncCronJobDto syncCronJobDto = new SyncCronJobDto(cronTaskConfiguration, SyncCornJobEnum.DELETE);
                clusterSyncService.syncCronJob(syncCronJobDto);
            }
            UUID uuid = cronTaskConfigurationService.saveConfiguration(cronTaskConfiguration);
            cronTaskConfiguration.setUuid(uuid);
            SyncCronJobDto syncCronJobDto = new SyncCronJobDto(cronTaskConfiguration, SyncCornJobEnum.ADD_OR_UPDATE);
            clusterSyncService.syncCronJob(syncCronJobDto);
        } catch (Exception e) {
            log.error(e.getMessage(), e);
            throw new RuntimeException(e.getMessage(), e);
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
            log.error("Update mirror error [{}]", ExceptionUtils.getStackTrace(e));
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

    /**
     * 全量制品扫描
     *
     * @param vulnerabilityRefreshTime 漏洞数据更新时间
     */
    private void artifactsFullScan(LocalDateTime vulnerabilityRefreshTime) {
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
        Checksum.clearCache();
    }

    public void artifactsScan() {
        List<String> safeLevels = Lists.newArrayList(SafeLevelEnum.INIT.getLevel(), SafeLevelEnum.SCANNING.getLevel(), SafeLevelEnum.SCAN_FAIL.getLevel(), SafeLevelEnum.UN_SCAN.getLevel());
        artifactsScan(safeLevels, Order.desc.name());
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
                Checksum.clearCache();
                log.info("Scan thread name [{}] time [{}]", Thread.currentThread().getName(), DateUtil.now());
            } finally {
                distributedLockComponent.unLock(lockName, 3500L);
            }
        } else {
            log.info("LockName [{}] was not get lock", lockName);
        }
    }

    private List<String> getScanStorageIdAndRepositoryIdList() {
        Example example = new Example(ScanRules.class);
        example.createCriteria().andEqualTo("onScan", 1);
        List<ScanRules> scanRulesList = scanRulesMapper.selectByExample(example);
        if (CollectionUtils.isEmpty(scanRulesList)) {
            return null;
        }
        return scanRulesList.stream().map(item -> String.format("%s-%s", item.getStorage(), item.getRepository())).collect(Collectors.toList());
    }

    private String getRetryKey() {
        String retryKey = GlobalConstants.SCAN_RETRY;
        String cacheKey = distributedCacheComponent.get(GlobalConstants.SCAN_RETRY_KEY);
        if (StringUtils.isNotBlank(cacheKey)) {
            retryKey = cacheKey;
        }
        return retryKey;
    }

    private Integer getRetryCount() {
        Integer retryCount = GlobalConstants.SCAN_RETRY_COUNT;
        String cacheKey = distributedCacheComponent.get(GlobalConstants.SCAN_RETRY_COUNT_KEY);
        if (StringUtils.isNotBlank(cacheKey)) {
            retryCount = Integer.parseInt(cacheKey);
        }
        return retryCount;
    }

    public boolean validateRepositoryScan(String storageId, String repositoryId) {
        Example example = new Example(ScanRules.class);
        example.createCriteria().andEqualTo("id", String.format("%s-%s", storageId, repositoryId));
        example.createCriteria().andEqualTo("onScan", 1);
        List<ScanRules> scanRulesList = scanRulesMapper.selectByExample(example);
        if (CollectionUtils.isEmpty(scanRulesList)) {
            return false;
        }
        return true;
    }

    private double getLevenshteinDistanceSimilarValue() {
        double similarValue = 10;
        String key = "LICENSE_LEVENSHTEIN_SIMILAR_VALUE";
        String cacheValue = distributedCacheComponent.get(key);
        if (StringUtils.isNotBlank(cacheValue)) {
            similarValue = Double.parseDouble(cacheValue);
        }
        return similarValue;
    }

    private double getJaccardSimilarValue() {
        double similarValue = 0.85;
        String key = "LICENSE_JACCARD_SIMILAR_VALUE";
        String cacheValue = distributedCacheComponent.get(key);
        if (StringUtils.isNotBlank(cacheValue)) {
            similarValue = Double.parseDouble(cacheValue);
        }
        return similarValue;
    }
}
