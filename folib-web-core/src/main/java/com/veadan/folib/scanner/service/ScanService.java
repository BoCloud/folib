package com.veadan.folib.scanner.service;


import cn.hutool.core.date.DatePattern;
import cn.hutool.core.date.DateUtil;
import cn.hutool.core.io.FileUtil;
import cn.hutool.json.JSONUtil;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONException;
import com.alibaba.fastjson.JSONObject;
import com.alibaba.fastjson.serializer.SerializerFeature;
import com.beust.jcommander.internal.Sets;
import com.veadan.folib.cloud.storage.s3fs.S3Path;
import com.veadan.folib.components.artifact.ArtifactComponent;
import com.veadan.folib.components.license.LicenseComponent;
import com.veadan.folib.components.scan.ScanComponent;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.Component;
import com.veadan.folib.domain.ComponentEntity;
import com.veadan.folib.domain.VulnerabilityEntity;
import com.veadan.folib.entity.Dict;
import com.veadan.folib.entity.License;
import com.veadan.folib.enums.DictTypeEnum;
import com.veadan.folib.enums.SafeLevelEnum;
import com.veadan.folib.enums.VulnerabilityPlatformEnum;
import com.veadan.folib.event.artifact.ArtifactEventTypeEnum;
import com.veadan.folib.eventlistener.scanner.ArtifactEventScannerListener;
import com.veadan.folib.forms.dict.DictForm;
import com.veadan.folib.forms.scanner.ScannerReportForm;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.layout.DockerFileSystem;
import com.veadan.folib.repositories.ComponentRepository;
import com.veadan.folib.scanner.common.exception.BusinessException;
import com.veadan.folib.scanner.common.util.DateUtils;
import com.veadan.folib.scanner.config.ScanConfig;
import com.veadan.folib.scanner.entity.ScannerReport;
import com.veadan.folib.scanner.enums.SeverityTypeEnum;
import com.veadan.folib.scanner.mapper.ScanRulesMapper;
import com.veadan.folib.services.ArtifactService;
import com.veadan.folib.services.DictService;
import com.veadan.folib.services.VulnerabilityService;
import com.veadan.folib.services.VulnerabilityWebService;
import com.veadan.folib.util.FileSizeConvertUtils;
import com.veadan.folib.util.LocalDateTimeInstance;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.compress.utils.Lists;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.owasp.dependencycheck.data.update.exception.UpdateException;
import org.owasp.dependencycheck.dependency.*;
import org.owasp.dependencycheck.dependency.naming.Identifier;
import org.owasp.dependencycheck.dependency.naming.PurlIdentifier;
import org.owasp.dependencycheck.utils.Checksum;
import org.owasp.dependencycheck.utils.Settings;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

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
    private ArtifactEventScannerListener artifactEventScannerListener;

    @Inject
    private ScanComponent scanComponent;

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
                //文件大于3GB，放弃扫描
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
            Dependency[] dependencies = null;
            for (String filePath : filePaths) {
                filePath = parseFilePath(filePath);
                //执行扫描
                dependencies = scanWorker(artifact, filePath);
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
            buildReport(artifact, dependencyList);
            dependencyList.clear();
            dependencyList = null;
        } catch (Exception e) {
            artifact.setSafeLevel(SafeLevelEnum.SCAN_FAIL.getLevel());
            artifactService.saveOrUpdateArtifact(artifact);
            log.error("执行扫描失败：{}", ExceptionUtils.getStackTrace(e));
        }
        artifact.setReport("");
    }

    private boolean checkSize(long sizeInBytes) {
        BigDecimal maxSize = new BigDecimal(3);
        BigDecimal convertSize = FileSizeConvertUtils.convertBytesWithDecimal(sizeInBytes, "GB");
        if (convertSize.compareTo(maxSize) > 0) {
            return false;
        }
        return true;
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


    public Dependency[] scanWorker(Artifact artifact, String filePath) {
        String parentPath = null;
        XpEngine engine = null;
        try {
            engine = new XpEngine(getSettings());
            RepositoryPath repositoryPath = resolvePath(artifact);
            if (repositoryPath.getTarget() instanceof S3Path) {
                Path artifactPath;
                S3Path s3RepositoryPath = (S3Path) repositoryPath.getTarget();
                parentPath = tempPath + File.separator + UUID.randomUUID();
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
            log.info("Scan file path [{}]", filePath);
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
            if (Objects.nonNull(parentPath)) {
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

    private void buildReport(Artifact artifact, List<Dependency> dependencyList) {
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
            buildComponent(dependency, componentSet);
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
     * @param dependency   dependency
     * @param componentSet componentSet
     */
    private void buildComponent(Dependency dependency, Set<Component> componentSet) {
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
        List<License> licenses = licenseComponent.getLicenses();
        component.setDescription(dependency.getDescription());
        component.setMd5sum(dependency.getMd5sum());
        component.setSha256sum(dependency.getSha256sum());
        if (CollectionUtils.isNotEmpty(licenses)) {
            if (StringUtils.isNotBlank(dependency.getLicense())) {
                log.info("Dependency license [{}]", dependency.getLicense());
                String[] dependencyLicenses = dependency.getLicense().split(",");
                Set<String> licenseSet = Sets.newLinkedHashSet();
                for (String license : dependencyLicenses) {
                    licenseSet.addAll(licenses.stream().filter(item -> StringUtils.isNotBlank(item.getLicenseUrl())).filter(item -> Arrays.stream(item.getLicenseUrl().split(",")).anyMatch(license::contains)).map(License::getLicenseId).collect(Collectors.toSet()));
                }
                log.info("LicenseSet {}", licenseSet);
                component.setLicense(licenseSet);
            }
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
            dictService.updateDict(DictForm.builder().id(dict.getId()).comment("更新完成").build());
        } catch (UpdateException e) {
            dictService.updateDict(DictForm.builder().id(dict.getId()).comment("更新错误").build());
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

}
