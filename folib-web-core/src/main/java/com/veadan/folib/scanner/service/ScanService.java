package com.veadan.folib.scanner.service;


import cn.hutool.core.io.FileUtil;
import cn.hutool.core.io.file.PathUtil;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONException;
import com.beust.jcommander.internal.Sets;
import com.veadan.folib.cloud.storage.s3fs.S3Path;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.VulnerabilityEntity;
import com.veadan.folib.enums.SafeLevelEnum;
import com.veadan.folib.enums.VulnerabilityPlatformEnum;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.layout.DockerFileSystem;
import com.veadan.folib.scanner.biz.FolibScannerBiz;
import com.veadan.folib.scanner.biz.ScanRulesBiz;
import com.veadan.folib.scanner.common.constant.ScanConstans;
import com.veadan.folib.scanner.common.exception.BusinessException;
import com.veadan.folib.scanner.config.ScanConfig;
import com.veadan.folib.scanner.entity.FolibScanner;
import com.veadan.folib.scanner.entity.ScanRules;
import com.veadan.folib.scanner.enums.SeverityTypeEnum;
import com.veadan.folib.scanner.mapper.FolibScannerMapper;
import com.veadan.folib.services.ArtifactResolutionService;
import com.veadan.folib.services.ArtifactService;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.services.VulnerabilityService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.compress.utils.Lists;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.owasp.dependencycheck.data.update.exception.UpdateException;
import org.owasp.dependencycheck.dependency.*;
import org.owasp.dependencycheck.utils.Settings;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import software.amazon.awssdk.services.s3.model.GetObjectRequest;

import javax.inject.Inject;
import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;
import java.util.stream.Collectors;

//import com.veadan.folib.scanner.common.util.file.TikaFileType;

@Slf4j
@Service
public class ScanService {

    @Autowired
    private ScanConfig scanConfig;

    @Autowired
    private ScanRulesBiz scanRulesBiz;

    @Autowired
    private FolibScannerMapper folibScannerMapper;

    @Autowired
    private FolibScannerBiz folibScannerBiz;

    @Inject
    protected ArtifactResolutionService artifactResolutionService;

    @Inject
    private VulnerabilityService vulnerabilityService;

    @Inject
    private ArtifactService artifactService;

    @Inject
    private ConfigurationManagementService configurationManagementService;

    @Value("${folib.temp}")
    private String tempPath;

    private Executor threadPool = Executors.newFixedThreadPool(100);

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
    public void asyncScan(FolibScanner folibScanner) {
        try {
            //将数据库中该记录变为扫描中
            folibScanner.setScanStatus(ScanConstans.SCANING);
            folibScannerBiz.updateSelectiveById(folibScanner);
            handlerArtifact(folibScanner, null, null, SafeLevelEnum.SCANNING);
            //执行扫描
            scanWorker(folibScanner);
        } catch (Exception e) {
            folibScanner.setScanStatus(ScanConstans.SCANFAILED);
            folibScannerBiz.updateSelectiveById(folibScanner);
            handlerArtifact(folibScanner, null, null, SafeLevelEnum.SCAN_FAIL);
            log.error("=====>>>>>执行扫描失败：{}", ExceptionUtils.getStackTrace(e));
            throw new BusinessException("文件解析失败");
        }

    }

    //扫描执行方法
    public void scanWorker(FolibScanner folibScanner) throws Exception {
        String parentPath = null;
        try {
            XpEngine engine = new XpEngine(getSettings());
            RepositoryPath repositoryPath = resolvePath(folibScanner);
            String filePath = "";
            if (repositoryPath.getTarget() instanceof S3Path) {
                S3Path s3RepositoryPath = (S3Path) repositoryPath.getTarget();
                parentPath = tempPath + File.separator + UUID.randomUUID();
                InputStream inputStream = null;
                //s3存储
                if (repositoryPath.getFileSystem() instanceof DockerFileSystem) {
                    String temp = folibScanner.getPath().substring(folibScanner.getPath().indexOf(repositoryPath.getStorageId()));
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
            } else {
                filePath = scanConfig.getWatchMonitorPath() + folibScanner.getPath();
            }
            engine.scan(filePath);
            engine.analyzeDependencies();
            Dependency[] dependencies = engine.getDependencies();
            folibScanner.setScanStatus(ScanConstans.SCANED);
            folibScanner.setScanTime(new Date());
            analysisReport(folibScanner, dependencies);
            folibScannerBiz.updateSelectiveById(folibScanner);
        } catch (Exception ex) {
            log.error("=====>>>>>scanWorker error：{}", ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException(ex);
        } finally {
            //删除临时文件
            if (Objects.nonNull(parentPath)) {
                FileUtil.del(new File(parentPath));
            }
        }

    }

    //开启扫描
    public void analysisReport(FolibScanner folibScanner, Dependency[] dependencies) {
        folibScannerBiz.updateSelectiveById(buildReport(folibScanner, dependencies));
    }

    //根据依赖构建并补充报告内容数据
    private FolibScanner buildReport(FolibScanner folibScanner, Dependency[] dependencyList) {
        int vulnCount = 0;
        int vulnSuppressedCount = 0;
        int cpeSuppressedCount = 0;
        int vulnDepCount = 0;
        List<Dependency> dependencyLists = Arrays.asList(dependencyList);
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
        folibScanner.setReport(JSONArray.toJSONString(dependencyLists));
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
        folibScanner.setVulnerabilitesCount(vulnCount);
        folibScanner.setVulnerableCount(vulnDepCount);
        folibScanner.setSuppressedCount(vulnSuppressedCount);
        folibScanner.setDependencyCount(dependencyList.length);
        folibScanner.setScanTime(new Date());
        handlerVulnerability(folibScanner, vulnerabilitySet);
        handlerArtifact(folibScanner, evidenceQuantity, vulnerabilitySet, SafeLevelEnum.SCAN_COMPLETE);
        return folibScanner;
    }

    private RepositoryPath resolvePath(FolibScanner folibScanner) throws IOException {
        String storageId = folibScanner.getStorage();
        String repositoryId = folibScanner.getRepository();
        String path = folibScanner.getPath();
        String temp = String.format("/%s/%s", storageId, repositoryId);
        String artifactPath = path.substring(path.indexOf(temp) + temp.length());
        if (artifactPath.startsWith("/")) {
            artifactPath = artifactPath.replaceFirst("/", "");
        }
        RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, artifactPath);
        if (Objects.isNull(repositoryPath) && StringUtils.isNotBlank(folibScanner.getArtifactPath())) {
            repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, folibScanner.getArtifactPath());
        }
        return repositoryPath;
    }

    /**
     * 更新制品扫描数据到图数据库
     *
     * @param folibScanner     扫描信息
     * @param evidenceQuantity 风险凭证个数
     * @param vulnerabilitySet 漏洞数据
     * @param safeLevelEnum    安全级别
     */
    private void handlerArtifact(FolibScanner folibScanner, Integer evidenceQuantity, Set<Vulnerability> vulnerabilitySet, SafeLevelEnum safeLevelEnum) {
        try {
            RepositoryPath repositoryPath = resolvePath(folibScanner);
            if (Objects.nonNull(repositoryPath)) {
                Artifact artifact = repositoryPath.getArtifactEntry();
                if (Objects.nonNull(artifact)) {
                    artifact.setSafeLevel(safeLevelEnum.getLevel());
                    artifact.setEvidenceQuantity(evidenceQuantity);
                    artifact.setDependencyCount(folibScanner.getDependencyCount());
                    artifact.setDependencyVulnerabilitiesCount(folibScanner.getVulnerableCount());
                    artifact.setVulnerabilitiesCount(folibScanner.getVulnerabilitesCount());
                    artifact.setSuppressedVulnerabilitiesCount(folibScanner.getSuppressedCount());
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
            }
        } catch (Exception ex) {
            log.error("=====>>>>>更新制品扫描数据到图数据库失败：{}", ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException(ex);
        }
    }

    /**
     * 更新制品扫描数据到图数据库
     *
     * @param folibScanner     扫描信息
     * @param vulnerabilitySet 漏洞数据
     */
    private void handlerVulnerability(FolibScanner folibScanner, Set<Vulnerability> vulnerabilitySet) {
        if (CollectionUtils.isNotEmpty(vulnerabilitySet)) {
            List<com.veadan.folib.domain.Vulnerability> vulnerabilityList = Lists.newArrayList();
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
                vulnerabilityList.add(vulnerabilityEntity);
            }
            vulnerabilityService.saveOrUpdateVulnerabilityBatch(vulnerabilityList);
        }
    }

    @Async("asyncThreadPoolTaskExecutor")
    public void updateDB() {
        Settings settings = getSettings();
        settings.setBoolean(Settings.KEYS.UPDATE_NVDCVE_ENABLED, true);
        settings.setBoolean(Settings.KEYS.AUTO_UPDATE, true);
        XpEngine engine = new XpEngine(settings);
        try {
            engine.doUpdates();
        } catch (UpdateException e) {
            throw new BusinessException("更新出错");
        }

    }

    public void checkScan(RepositoryPath repositoryPath, String type, String filePath, String artifactFilePath) {
        Path path = repositoryPath.getTarget();
        String pathLike = "";
        String storageId = repositoryPath.getStorageId();
        String repository = repositoryPath.getRepositoryId();
        ScanRules scanRules = scanRulesBiz.selectById(storageId + "-" + repository);
        boolean onScan = false;
        if (Objects.nonNull(scanRules)) {
            onScan = scanRules.getOnScan();
        }
        log.debug("=====>>>>>存储空间：{}，仓库：{}，扫描开启状态 ：{}", storageId, repository, onScan);
        if (StringUtils.isBlank(filePath)) {
            filePath = repositoryPath.toAbsolutePath().toString();
        }
        FolibScanner folibScanner = null;
        if (path instanceof S3Path) {
            log.debug("=====>>>>>S3存储");
            S3Path s3Path = (S3Path) path;
            folibScanner = buildFolibScanner(storageId, repository, s3Path);
            if (Objects.nonNull(folibScanner)) {
                folibScanner.setPath(filePath);
                if (StringUtils.isBlank(folibScanner.getFileType())) {
                    folibScanner.setFileType(FileUtil.getSuffix(filePath));
                }
            }
            pathLike = repositoryPath.toString();
        } else {
            log.debug("=====>>>>>文件存储");
            folibScanner = buildFolibScanner(storageId, repository, filePath);
            pathLike = "storages" + repositoryPath.toUri().getPath();
        }
        if (StringUtils.isNotBlank(artifactFilePath)) {
            String artifactPath;
            if (path instanceof S3Path) {
                artifactPath = artifactFilePath.replace(String.format("%s/%s/", storageId, repository), "");
            } else {
                artifactFilePath = artifactFilePath.substring(artifactFilePath.indexOf("/storages/"));
                artifactPath = artifactFilePath.replace(String.format("/storages/%s/%s/", storageId, repository), "");
            }
            if (Objects.nonNull(folibScanner)) {
                folibScanner.setArtifactPath(artifactPath);
            }
        }
        if (ScanConstans.ADD.equals(type)) {
            if (Objects.nonNull(folibScanner)) {
                saveScanningData(folibScanner);
            }
        } else if (ScanConstans.DEL.equals(type)) {
            if (Objects.nonNull(folibScanner)) {
                deleteScanningData(folibScanner);
            }
        } else if (ScanConstans.DEL_DIRECTORY.equals(type)) {
            deleteScanningDataLike(storageId, repository, pathLike);
        } else {
            if (Objects.nonNull(folibScanner)) {
                updateScanningData(folibScanner);
            }
        }
    }

    private FolibScanner buildFolibScanner(String storageId, String repository, String path) {
        if (!FileUtil.isDirectory(path)) {
            File file = new File(path);
            String type = "";
            if (file.exists()) {
                //先取后缀
                type = FileUtil.getSuffix(file);
                if (StringUtils.isBlank(type)) {
                    //后缀无法获取，使用魔法值获取类型
                    type = FileUtil.getType(file);
                }
            }
            String shortPath = path.substring(path.lastIndexOf("storages/"));
            FolibScanner folibScanner = new FolibScanner();
            folibScanner.setPath(shortPath)
                    .setFileType(type).setRepository(repository)
                    .setStorage(storageId);
            ScanRules scanRules = scanRulesBiz.selectById(storageId + "-" + repository);
            boolean flag = false;
            if (Objects.nonNull(scanRules)) {
                flag = scanRules.getOnScan();
            }
            folibScanner.setOnScan(flag);
            return folibScanner;
        } else {
            return null;
        }
    }

    private FolibScanner buildFolibScanner(String storageId, String repository, S3Path s3Path) {
        if (!PathUtil.isDirectory(s3Path)) {
            String s3FilePath = s3Path.toString();
            FolibScanner folibScanner = new FolibScanner();
            String type = FileUtil.getSuffix(s3FilePath);
            folibScanner.setFileType(type).setRepository(repository)
                    .setStorage(storageId);
            ScanRules scanRules = scanRulesBiz.selectById(storageId + "-" + repository);
            boolean flag = false;
            if (Objects.nonNull(scanRules)) {
                flag = scanRules.getOnScan();
            }
            folibScanner.setOnScan(flag);
            return folibScanner;
        } else {
            return null;
        }
    }

    //新增制品是新增持久化，并设为没有进行安全扫描
    public void saveScanningData(FolibScanner folibScanner) {

        FolibScanner folib = folibScannerBiz.selectById(folibScanner.getPath());
        //数据库中如果不存在则保存该文件

        if (folib == null) {
            folibScannerBiz.insertSelective(folibScanner.setScanStatus(ScanConstans.UNSCAN));
        }
    }

    public void updateScanningData(FolibScanner folibScanner) {
        FolibScanner folib = folibScannerBiz.selectById(folibScanner.getPath());
        if (folib != null) {
            folibScannerBiz.updateSelectiveById(folibScanner.setScanStatus(ScanConstans.UNSCAN));
        } else {
            folibScannerBiz.insertSelective(folibScanner.setScanStatus(ScanConstans.UNSCAN));
        }
    }

    public void deleteScanningData(FolibScanner folibScanner) {
        FolibScanner folib = folibScannerBiz.selectById(folibScanner.getPath());
        if (folib != null) {
            folibScannerBiz.delete(folibScanner);
        }
    }

    public void deleteScanningDataLike(String storage, String repository, String path) {
        folibScannerMapper.deleteByPathLike(storage, repository, path);
    }

    @Async("asyncThreadPoolTaskExecutor")
    public void scanByScanRules(ScanRules scanRules) {
        if (scanRules.getOnScan()) {
            String path = scanConfig.getWatchMonitorPath() + "storages/" + scanRules.getStorage() + "/" + scanRules.getRepository() + "/";
            List<File> files = FileUtil.loopFiles(new File(path), new FileFilter() {
                @Override
                public boolean accept(File pathname) {
                    if (pathname.getName().contains(".jar") && !pathname.getName().contains(".md5") && !pathname.getName().contains(".sha1")) {
                        return true;
                    } else {
                        return false;
                    }
                }
            });
            for (File file : files) {
                FolibScanner folibScanner = buildFolibScanner(scanRules.getStorage(), scanRules.getRepository(), file.getPath());
                folibScanner.setOnScan(scanRules.getOnScan());
//           Long q=folibScannerBiz.selectCount(folibScanner);
                saveScanningData(folibScanner);
            }
            folibScannerMapper.updateByStorage(true, scanRules.getRepository(), scanRules.getStorage());
        } else {
            folibScannerMapper.updateByStorage(false, scanRules.getRepository(), scanRules.getStorage());

        }
//        return files;
    }
}
