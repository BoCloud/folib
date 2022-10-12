package com.veadan.folib.scanner.service;


import cn.hutool.core.io.FileUtil;
import cn.hutool.core.io.IoUtil;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONException;
import com.beust.jcommander.internal.Sets;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.VulnerabilityEntity;
import com.veadan.folib.enums.SafeLevelEnum;
import com.veadan.folib.enums.VulnerabilityPlatformEnum;
import com.veadan.folib.providers.io.RepositoryPath;
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
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import javax.inject.Inject;
import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.nio.file.Path;
import java.nio.file.WatchEvent;
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
        XpEngine engine = new XpEngine(getSettings());
        engine.scan(scanConfig.getWatchMonitorPath() + folibScanner.getPath());
        engine.analyzeDependencies();
        Dependency[] dependencies = engine.getDependencies();
        folibScanner.setScanStatus(ScanConstans.SCANED);
        folibScanner.setScanTime(new Date());
        analysisReport(folibScanner, dependencies);
        folibScannerBiz.updateSelectiveById(folibScanner);
        //删除临时文件
//
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
            String storageId = folibScanner.getStorage();
            String repositoryId = folibScanner.getRepository();
            String path = folibScanner.getPath();
            String artifactPath = path.replace(String.format("storages/%s/%s", storageId, repositoryId), "").replaceFirst("/", "");
            RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, artifactPath);
            if (Objects.nonNull(repositoryPath)) {
                Artifact artifact = repositoryPath.getArtifactEntry();
                if (Objects.nonNull(artifact)) {
                    artifact.setSafeLevel(safeLevelEnum.getLevel());
                    if (CollectionUtils.isNotEmpty(vulnerabilitySet)) {
                        Set<String> vulnerabilityNameSet = vulnerabilitySet.stream().map(Vulnerability::getName).collect(Collectors.toSet());
                        artifact.setEvidenceQuantity(evidenceQuantity);
                        artifact.setVulnerabilities(vulnerabilityNameSet);
                        artifact.setDependencyCount(folibScanner.getDependencyCount());
                        artifact.setDependencyVulnerabilitiesCount(folibScanner.getVulnerableCount());
                        artifact.setVulnerabilitiesCount(folibScanner.getVulnerabilitesCount());
                        artifact.setSuppressedVulnerabilitiesCount(folibScanner.getSuppressedCount());
                        long critical = vulnerabilitySet.stream().filter(item -> SeverityTypeEnum.CRITICAL.getType().equals(item.getHighestSeverityText())).count();
                        artifact.setCriticalVulnerabilitiesCount((int) critical);
                        long high = vulnerabilitySet.stream().filter(item -> SeverityTypeEnum.HIGH.getType().equals(item.getHighestSeverityText())).count();
                        artifact.setHighVulnerabilitiesCount((int) high);
                        long medium = vulnerabilitySet.stream().filter(item -> SeverityTypeEnum.MEDIUM.getType().equals(item.getHighestSeverityText())).count();
                        artifact.setMediumVulnerabilitiesCount((int) medium);
                        long low = vulnerabilitySet.stream().filter(item -> SeverityTypeEnum.LOW.getType().equals(item.getHighestSeverityText())).count();
                        artifact.setLowVulnerabilitiesCount((int) low);
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

    ///Users/veadan/IdeaProjects/folib2/folib-server/folib-vault/storages/folib-common/aliyun-maven/com/alibaba/fastjson/1.2.70-> fastjson-1.2.70.jar
    public boolean checkScan(WatchEvent<?> event, Path currentPath, String type) {
        String filePath = currentPath.toString() + "/" + event.context();
        File file = new File(filePath);

        if (FileUtil.isNotEmpty(file) && !FileUtil.isDirectory(file)) {
            //如果是jar包则存入待扫描区域
            FolibScanner folibScanner = buildFolibScanner(filePath);
            if (folibScanner.getFileType().equals(".jar")) {
                if (ScanConstans.ADD.equals(type)) {
                    saveScanningData(folibScanner);
                } else if (ScanConstans.UPDATE.equals(type)) {
                    updateScanningData(folibScanner);
                } else if (ScanConstans.DEL.equals(type)) {
                    deleteScanningData(folibScanner);
                } else if (ScanConstans.OVERFLOW.equals(type)) {
                    updateScanningData(folibScanner);
                }
                return true;
            }
        }
        return false;
    }

    public void checkScan(RepositoryPath repositoryPath, String type, String filePath) {
        String path = "storages" + repositoryPath.toUri().getPath();
        String storagesName = repositoryPath.getStorageId();
        String repository = repositoryPath.getRepositoryId();
        ScanRules scanRules = scanRulesBiz.selectById(storagesName + "-" + repository);
        boolean onScan = false;
        if (Objects.nonNull(scanRules)) {
            onScan = scanRules.isOnScan();
        }
        log.info("=====>>>>>存储空间：{}，仓库：{}，扫描开启状态 ：{}", storagesName, repository, onScan);
        if (StringUtils.isBlank(filePath)) {
            filePath = repositoryPath.toAbsolutePath().toString();
        }
        FolibScanner folibScanner = buildFolibScanner(filePath);
        if (ScanConstans.ADD.equals(type)) {
            if (Objects.nonNull(folibScanner)) {
                saveScanningData(folibScanner);
            }
        } else if (ScanConstans.DEL.equals(type)) {
            if (Objects.nonNull(folibScanner)) {
                deleteScanningData(folibScanner);
            }
        } else if (ScanConstans.DEL_DIRECTORY.equals(type)) {
            deleteScanningDataLike(storagesName, repository, path);
        } else {
            if (Objects.nonNull(folibScanner)) {
                updateScanningData(folibScanner);
            }
        }
    }

    private FolibScanner buildFolibScanner(String path) {
        if (!FileUtil.isDirectory(path)) {
            //获取path,从storages往后的
            String[] pathArray = path.split("/");
            int storagesIndex = 0;
            for (int i = 0; i < pathArray.length; i++) {
                if (pathArray[i].equals("storages")) {
                    storagesIndex = i;
                }
            }
            //获取存储空间名称和仓库名称
            String storagesName = pathArray[storagesIndex + 1];
            String repository = pathArray[storagesIndex + 2];
            File file = new File(path);
            String type = "";
            if (file.exists()) {
                //先取后缀
                type = FileUtil.getSuffix(file);
                if (StringUtils.isBlank(type)) {
                    //后缀无法获取，使用魔法值获取类型
                    type = FileUtil.getType(file);
                }
                try {
                    String hex = IoUtil.readHex28Lower(new FileInputStream(file));
                    log.info("=====>>>>> 路径：{}，类型：{}，hex：{}", file.getName(), type, hex);
                } catch (Exception ex) {
                    log.error("=====>>>>>读取魔数类型失败：{}", ExceptionUtils.getStackTrace(ex));
                }
            }
            String shortPath = path.substring(path.lastIndexOf("storages/"));
            FolibScanner folibScanner = new FolibScanner();

            folibScanner.setPath(shortPath)
                    .setFileType(type).setRepository(repository)
                    .setStorage(storagesName);
            ScanRules scanRules = scanRulesBiz.selectById(storagesName + "-" + repository);
            boolean flag = false;
            if (Objects.nonNull(scanRules)) {
                flag = scanRules.isOnScan();
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
        if (scanRules.isOnScan()) {
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
                FolibScanner folibScanner = buildFolibScanner(file.getPath());
                folibScanner.setOnScan(scanRules.isOnScan());
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
