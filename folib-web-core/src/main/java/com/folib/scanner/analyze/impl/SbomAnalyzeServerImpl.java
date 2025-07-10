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
package com.folib.scanner.analyze.impl;

import cn.hutool.core.util.StrUtil;
import com.folib.services.*;
import com.github.packageurl.MalformedPackageURLException;
import com.github.packageurl.PackageURL;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.folib.components.artifact.ArtifactComponent;
import com.folib.components.license.LicenseComponent;
import com.folib.configuration.ConfigurationManager;
import com.folib.constant.GlobalConstants;
import com.folib.domain.Artifact;
import com.folib.domain.ComponentEntity;
import com.folib.domain.VulnerabilityEntity;
import com.folib.enums.SafeLevelEnum;
import com.folib.enums.VulnerabilityPlatformEnum;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.repositories.ComponentRepository;
import com.folib.scanner.analysis.BomAnalysisFactory;
import com.folib.scanner.analyze.SbomAnalyzeServer;
import com.folib.scanner.common.util.DateUtils;
import com.folib.scanner.entity.ScannerReport;
import com.folib.scanner.enums.SeverityTypeEnum;
import com.folib.scanner.enums.Tools;
import com.folib.scanner.utils.CycloneDxUtls;
import com.folib.util.LocalDateTimeInstance;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.cyclonedx.Version;
import org.cyclonedx.exception.GeneratorException;
import org.cyclonedx.generators.json.BomJsonGenerator;
import org.cyclonedx.model.*;
import org.cyclonedx.model.vulnerability.Vulnerability;
import org.cyclonedx.parsers.JsonParser;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;

import javax.inject.Inject;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.security.MessageDigest;
import java.time.LocalDateTime;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

@Slf4j
@Service
public class SbomAnalyzeServerImpl implements SbomAnalyzeServer {

    @Inject
    @Lazy
    private LicenseComponent licenseComponent;
    @Inject
    @Lazy
    private VulnerabilityService vulnerabilityService;
    @Inject
    @Lazy
    private VulnerabilityDatabaseService vulnerabilityDatabaseService;
    @Inject
    @Lazy
    private ComponentRepository componentRepository;
    @Inject
    private ArtifactService artifactService;
    @Inject
    protected RepositoryPathResolver repositoryPathResolver;
    @Inject
    @Lazy
    private ArtifactComponent artifactComponent;
    @Inject
    @Lazy
    private VulnerabilityWebService vulnerabilityWebService;
    @Inject
    @Lazy
    protected ArtifactManagementService artifactManagementService;
    @Inject
    @Lazy
    private BomAnalysisFactory bomAnalysisFactory;
    @Lazy
    @Inject
    protected ConfigurationManager configurationManager;

    /**
     * 分析sbom
     *
     * @param bom sbom
     */
    @Override
    public void analyzeCycloneDxBom(Bom bom) throws Exception {

        if (bom.getMetadata() == null || bom.getMetadata().getTools() == null) {
            log.error("sbom metadata is null or name is null");
            return;
        }
        log.info("sbom artifact bomId:{} is start analyze", bom.getMetadata().getComponent().getName());
        String tools = bom.getMetadata().getTools().get(0).getName();
        String artifactUUID = bom.getMetadata().getComponent().getName();
        Optional<Artifact> optionalArtifact = Optional.ofNullable(getArtifact(artifactUUID));
        if (optionalArtifact.isEmpty()) {
            log.error("sbom artifact UUID:{} is not exist", artifactUUID);
            return;
        }
        Artifact artifact = optionalArtifact.get();
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(artifact.getStorageId(), artifact.getRepositoryId(), artifact.getArtifactPath());
        RepositoryPath bomPath = artifactComponent.getBomRepositoryPath(repositoryPath);
        if (!Files.exists(bomPath)) {
            log.error("sbom artifact pat:{} is not exist", bomPath.getPath());
            return;
        }
        JsonParser jsonParser = new JsonParser();
        Bom sourceBom = jsonParser.parse(Files.readAllBytes(bomPath));
        //区分bom分析平台
        if (Tools.QAXOSS.getValue().equals(tools)) {
            log.info("sbom artifact UUID:{} is qaXoss", artifactUUID);
            CycloneDxUtls.mergeQanxinCycloneDx(bom, sourceBom);
        } else {
            CycloneDxUtls.mergeFoEyesCycloneDx(bom, sourceBom);
            //todo 其它平台待定
        }
        this.analyzeCycloneDx(sourceBom, false);

    }


    /**
     * 分析sbom
     *
     * @param bom sbom
     */
    @Override
    public void analyzeCycloneDx(Bom bom, Boolean isAnalysis) throws GeneratorException, IOException {
        LocalDateTime now = LocalDateTimeInstance.now();

        //分析组件
        Set<com.folib.domain.Component> entityList = analysisComponent(bom, now);
        String artifactUUID = bom.getMetadata().getComponent().getName();
        Artifact artifact = getArtifact(artifactUUID);
        if (artifact == null) {
            log.error("sbom artifact UUID:{} is not exist", artifactUUID);
            return;
        }

        if (!isAnalysis) {
            //漏洞保存到图库
            List<com.folib.domain.Vulnerability> vulnerabilities = handlerVulnerability(artifact, bom);
            //依赖数量
            Integer dependencyCount = bom.getDependencies() == null ? 0 : bom.getDependencies().size();
            //有漏洞的依赖数量
            int dependencyVulnerabilitiesCount = 0;
            Set<String> keys = new HashSet<>();
            if (bom.getVulnerabilities() != null && !bom.getVulnerabilities().isEmpty()) {
                for (Vulnerability vuln : bom.getVulnerabilities()) {
                    keys.addAll(vuln.getAffects().stream().map(Vulnerability.Affect::getRef).collect(Collectors.toSet()));
                }
            }
            dependencyVulnerabilitiesCount = keys.size();
            //漏洞数量
            Integer vulnerabilitiesCount = bom.getVulnerabilities() == null ? 0 : bom.getVulnerabilities().size();
            // todo
            int suppressedCount = 0;
            Integer evidenceQuantity = 0;

            handlerArtifact(artifact, dependencyCount, dependencyVulnerabilitiesCount, vulnerabilitiesCount,
                    suppressedCount, evidenceQuantity, new HashSet<>(vulnerabilities), SafeLevelEnum.SCAN_COMPLETE, entityList);
        }
        //组件保存到图库
        handlerComponent(entityList);
        //更新sbom json
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(artifact.getStorageId(), artifact.getRepositoryId(), artifact.getArtifactPath());
        String metadataPath = String.format("/.%s%s/bom.json", artifact.getArtifactName(), GlobalConstants.FO_LIBRARY_METADATA);
        String metadtaFilePath = repositoryPath.getPath().replace("/" + artifact.getArtifactName(), metadataPath);
        repositoryPath = repositoryPathResolver.resolve(artifact.getStorageId(), artifact.getRepositoryId(), metadtaFilePath);
        BomJsonGenerator generator = new BomJsonGenerator(bom, Version.VERSION_15);
        artifactManagementService.store(repositoryPath, new ByteArrayInputStream(generator.toJsonString().getBytes(StandardCharsets.UTF_8)));
        if (isAnalysis) {
            bomAnalysisFactory.analysis(String.format("%s:%s:%s", artifact.getStorageId(), artifact.getRepositoryId(), artifact.getArtifactPath()), repositoryPath);
        }

    }

    @Override
    public void analyzeCycloneDx(Bom bom, boolean success, String projectId, String message, int priority, String taskName) throws Exception {
        if (success) {
            analyzeCycloneDxBom(bom);
        } else {
            Optional<Artifact> optionalArtifact = Optional.ofNullable(getArtifact(taskName));
            if (optionalArtifact.isEmpty()) {
                log.error("sbom artifact UUID:{} is not exist", taskName);
                return;
            }
            Artifact artifact = optionalArtifact.get();
            artifact.setSafeLevel(SafeLevelEnum.SCAN_FAIL.getLevel());
            artifactService.saveOrUpdateArtifact(artifact);
            log.error("执行扫描失败 [{}]", message);
        }
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
    private void handlerArtifact(Artifact artifact, Integer dependencyCount, Integer dependencyVulnerabilitiesCount, Integer vulnerabilitiesCount,
                                 Integer suppressedCount, Integer evidenceQuantity, Set<com.folib.domain.Vulnerability> vulnerabilitySet,
                                 SafeLevelEnum safeLevelEnum, Set<com.folib.domain.Component> componentSet) {
        try {
            if (Objects.nonNull(artifact)) {
                artifact.setSafeLevel(safeLevelEnum.getLevel());
                artifact.setEvidenceQuantity(evidenceQuantity);
                artifact.setDependencyCount(dependencyCount);
                artifact.setDependencyVulnerabilitiesCount(dependencyVulnerabilitiesCount);
                artifact.setVulnerabilitiesCount(vulnerabilitiesCount);
                artifact.setSuppressedVulnerabilitiesCount(suppressedCount);
                if (CollectionUtils.isNotEmpty(vulnerabilitySet)) {
                    Set<String> vulnerabilityNameSet = vulnerabilitySet.stream().map(com.folib.domain.Vulnerability::getUuid).collect(Collectors.toSet());
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
                    List<com.folib.domain.Vulnerability> vulnerabilityList = Lists.newArrayList();
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
     * 组件保存到图库
     *
     * @param componentSet componentSet
     */
    private void handlerComponent(Set<com.folib.domain.Component> componentSet) {
        if (CollectionUtils.isNotEmpty(componentSet)) {
            componentSet.forEach(component -> {
                componentRepository.saveOrUpdate(component);
            });
        }
    }

    public List<com.folib.domain.Vulnerability> handlerVulnerability(Artifact artifact, Bom bom) {
        Set<String> storages = Sets.newLinkedHashSet();
        Set<String> storagesAndRepositories = Sets.newLinkedHashSet();
        List<com.folib.domain.Vulnerability> vulnerabilityList = Lists.newArrayList();
        if (bom.getVulnerabilities() == null || bom.getVulnerabilities().isEmpty()) {
            return vulnerabilityList;
        }
        for (Vulnerability vulnerability : bom.getVulnerabilities()) {
            //VulnerabilityDatabaseForm vulnerabilityDatabaseForm = vulnerabilityDatabaseService.getOneVulnerabilityDatabase(vulnerability.getId());
            VulnerabilityEntity vulnerabilityEntity = new VulnerabilityEntity();
            vulnerabilityEntity.setUuid(vulnerability.getId());
            vulnerabilityEntity.setVulnerabilityPlatformName(VulnerabilityPlatformEnum.NVD.getName());
            if (vulnerability.getRatings() != null) {
                vulnerabilityEntity.setCvssV2Score(hadlerVulnerabilityCVSSScore(vulnerability, Vulnerability.Rating.Method.CVSSV2));
                vulnerabilityEntity.setCvssV2Severity(hadlerVulnerabilityCVSSSeverity(vulnerability, Vulnerability.Rating.Method.CVSSV2));
                vulnerabilityEntity.setCvssV3Severity(hadlerVulnerabilityCVSSSeverity(vulnerability, Vulnerability.Rating.Method.CVSSV3));
                vulnerabilityEntity.setCvssV3Score(hadlerVulnerabilityCVSSScore(vulnerability, Vulnerability.Rating.Method.CVSSV3));
            }

            if (vulnerabilityEntity.getCvssV3Severity() != null) {
                vulnerabilityEntity.setHighestSeverityText(vulnerabilityEntity.getCvssV3Severity());
            }
            if (vulnerabilityEntity.getCvssV2Severity() != null) {
                vulnerabilityEntity.setHighestSeverityText(vulnerabilityEntity.getCvssV2Severity());
            }
            vulnerabilityEntity.setDescription(vulnerability.getDescription());
            if (Vulnerability.Rating.Severity.CRITICAL.equals(vulnerabilityEntity.getCvssV2Severity())) {
                vulnerabilityEntity.setHighestSeverityText(vulnerabilityEntity.getCvssV2Severity());
            } else if (Vulnerability.Rating.Severity.CRITICAL.equals(vulnerabilityEntity.getCvssV3Severity())) {
                vulnerabilityEntity.setHighestSeverityText(vulnerabilityEntity.getCvssV3Severity());
            }
            //todo 修复建议
            //VulnerableSoftware vulnerableSoftware = vulnerability.getMatchedVulnerableSoftware();
            //if (Objects.nonNull(vulnerableSoftware)) {
            //    vulnerabilityEntity.setVersionEndExcluding(vulnerableSoftware.getVersionEndExcluding());
            //}
            storages.add(artifact.getStorageId());
            vulnerabilityEntity.setStorages(storages);
            storagesAndRepositories.add(String.format("%s-%s", artifact.getStorageId(), artifact.getRepositoryId()));
            vulnerabilityEntity.setStoragesAndRepositories(storagesAndRepositories);
            vulnerabilityList.add(vulnerabilityEntity);
        }
        vulnerabilityService.saveOrUpdateVulnerabilityBatch(vulnerabilityList);
        return vulnerabilityList;
    }

    public String hadlerVulnerabilityCVSSSeverity(Vulnerability vulnerability, Vulnerability.Rating.Method method) {
        return vulnerability.getRatings().stream().filter(rating -> Objects.nonNull(rating.getSeverity()) && Objects.nonNull(rating.getMethod()) && rating.getMethod().equals(method)).map(rating -> rating.getSeverity().toString()).findFirst().orElse(null);
    }

    public String hadlerVulnerabilityCVSSScore(Vulnerability vulnerability, Vulnerability.Rating.Method method) {
        return vulnerability.getRatings().stream().filter(rating -> Objects.nonNull(rating.getScore()) && Objects.nonNull(rating.getMethod()) && rating.getMethod().equals(method)).map(rating -> rating.getScore().toString()).findFirst().orElse(null);
    }


    public Set<com.folib.domain.Component> analysisComponent(Bom bom, LocalDateTime now) {
        Set<com.folib.domain.Component> entityList = new HashSet<>();
        List<com.folib.entity.License> licensesList = licenseComponent.getLicenses();
        if(CollectionUtils.isEmpty(bom.getComponents())){
            return entityList;
        }
        for (Component component : bom.getComponents()) {
            ComponentEntity entity = new ComponentEntity();
            entity.setCreated(now);
            entity.setLastUpdated(now);
            entity.setName(component.getName());
            entity.setFileName(handleFileName(component.getPurl(), component));
            entity.setVersion(component.getVersion());
            entity.setGroupId(component.getGroup());
            entity.setDescription(component.getDescription());
            entity.setPurl(component.getPurl());
            entity.setUrl(component.getPurl());
            entity.setCpe(component.getCpe());

            if (component.getExternalReferences() != null && !component.getExternalReferences().isEmpty()) {
                Set<Hash> hashes = new HashSet<>();
                for (ExternalReference externalReference : component.getExternalReferences()) {
                    if (externalReference.getHashes() != null) {
                        hashes.addAll(externalReference.getHashes());
                    }
                }
                Map<String, String> map = hashes.stream().collect(Collectors.toMap(Hash::getAlgorithm, Hash::getValue));
                entity.setMd5sum(map.get("MD5"));
                entity.setSha1sum(map.get("SHA-1"));
                entity.setSha256sum(map.get("SHA-256"));
                entity.setUuid(map.get("SHA-1"));
            }
            if (component.getLicenses() != null) {
                entity.setLicense(getLicenses(licensesList, component));
            }

            if (entity.getUuid() == null) {
                String uuid = entity.getPurl() ==null ? getSHA1(String.format("%s:%s:%s",  component.getName(), component.getVersion(),component.getType().getTypeName())) : getSHA1(component.getPurl());
                entity.setUuid(uuid);
            }
            handleVulnerabilities(bom, component.getPurl(), entity);
            entityList.add(entity);
        }
        return entityList;
    }

    public Set<String> getLicenses(List<com.folib.entity.License> licensesList, Component component) {
        List<License> licensesUrl = component.getLicenses().getLicenses();
        Set<String> result = new HashSet<>();
        for (License choice : licensesUrl) {
            result.addAll(licensesList.stream().filter(license ->
                    StrUtil.equals(license.getLicenseName(), choice.getName()) |
                            StrUtil.equals(license.getLicenseId(), choice.getId()) |
                            StrUtil.equals(license.getLicenseId(), choice.getUrl())
            ).map(com.folib.entity.License::getLicenseId).collect(Collectors.toSet()));
        }
        return result;
    }

    public void handleVulnerabilities(Bom bom, String componentBomRef, ComponentEntity entity) {
        // 检查 bom 是否为 null
        if (bom == null || bom.getVulnerabilities() == null) {
            return; // 如果 bom 或 vulnerabilities 为空，直接返回
        }
        // 获取漏洞列表并过滤
        Set<String> vulnerabilities = bom.getVulnerabilities().stream()
                .filter(vulnerability -> {
                    // 检查 vulnerability 的 affects 是否为 null
                    if (vulnerability == null || vulnerability.getAffects() == null) {
                        return false;
                    }
                    // 调用 validateAffect 方法进行过滤
                    return validateAffect(vulnerability.getAffects(), componentBomRef);
                })
                .map(vulnerability -> {
                    // 检查 vulnerability 的 id 是否为 null
                    if (vulnerability.getId() == null) {
                        return null; // 返回 null 表示忽略该元素
                    }
                    return vulnerability.getId();
                })
                .filter(Objects::nonNull) // 去除 null 值
                .collect(Collectors.toSet());

        entity.setVulnerabilities(vulnerabilities);
        entity.setVulnerabilitiesCount(vulnerabilities.size());

        List<Vulnerability> vulnerabilityList = bom.getVulnerabilities().stream()
                .filter(vulnerability -> {
                    // 检查 vulnerability 的 affects 是否为 null
                    if (vulnerability == null || vulnerability.getAffects() == null) {
                        return false;
                    }
                    // 调用 validateAffect 方法进行过滤
                    return validateAffect(vulnerability.getAffects(), componentBomRef);
                })
                .filter(Objects::nonNull) // 去除 null 值
                .collect(Collectors.toList());
        entity.setCriticalVulnerabilitiesCount((int) count(vulnerabilityList, Vulnerability.Rating.Severity.CRITICAL));
        entity.setHighVulnerabilitiesCount((int) count(vulnerabilityList, Vulnerability.Rating.Severity.HIGH));
        entity.setMediumVulnerabilitiesCount((int) count(vulnerabilityList, Vulnerability.Rating.Severity.MEDIUM));
        entity.setLowVulnerabilitiesCount((int) count(vulnerabilityList, Vulnerability.Rating.Severity.LOW));
        entity.setSuppressedVulnerabilitiesCount(0);
    }

    public long count(List<Vulnerability> vulnerabilities, Vulnerability.Rating.Severity severity) {
        return vulnerabilities.stream().filter(vulnerability -> validateSeverity(vulnerability.getRatings(), severity)).count();
    }

    public boolean validateAffect(List<Vulnerability.Affect> affects, String componentBomRef) {
        // 检查参数是否为空，避免空指针异常
        if (affects == null || componentBomRef == null) {
            return false; // 如果参数为空，直接返回 false（假设需求是不存在匹配项）
        }
        // 使用 anyMatch 提高可读性和效率
        return affects.stream().anyMatch(affect -> componentBomRef.equals(affect.getRef()));
    }

    public boolean validateSeverity(List<Vulnerability.Rating> ratings, Vulnerability.Rating.Severity severity) {
        if (ratings == null || severity == null) {
            return false; // 如果参数为空，直接返回 false（假设需求是不存在匹配项）
        }
        return ratings.stream().anyMatch(rating -> severity.equals(rating.getSeverity()));
    }

    public String handleFileName(final String purl, Component component) {
        String fileName = null;
        if (StrUtil.isBlank(purl)) {
            return fileName;
        }
        try {
            PackageURL parsedPurl = new PackageURL(purl);
            // 非空检查并构建文件名
            String name = validateAndRetrieve(parsedPurl.getName(), "Name");
            name = name == null ? component.getName() : name;
            String version = validateAndRetrieve(parsedPurl.getVersion(), "Version");
            version = version == null ? component.getVersion() : version;
            String type = validateAndRetrieve(parsedPurl.getType(), "Type");
            type = type == null ? component.getType().getTypeName() : type;
            fileName = String.format("%s-%s.%s", name, version, type);
        } catch (MalformedPackageURLException e) {
            log.error("Invalid Package URL: " + purl);
            e.printStackTrace();
        }
        return fileName;
    }

    private String validateAndRetrieve(String value, String fieldName) {
        if (value == null || value.isEmpty()) {
            return null;
        }
        return value;
    }

    private ScannerReport resolveReport(Bom bom) {
        ScannerReport scannerReport = ScannerReport.builder().build();
        Integer vulnerabilitiesCount = 0;
        int dependencyCount = bom.getComponents().size();
        int dependencyVulnerabilitiesCount = 0;
        int suppressedVulnerabilitiesCount = 0;
        scannerReport.setDependencyCount(dependencyCount);
        scannerReport.setVulnerabilitiesCount(vulnerabilitiesCount);
        scannerReport.setDependencyVulnerabilitiesCount(dependencyVulnerabilitiesCount);
        scannerReport.setSuppressedVulnerabilitiesCount(suppressedVulnerabilitiesCount);
        Date now = new Date();
        scannerReport.setScanDate(DateUtils.getTodayDate());
        scannerReport.setScanDateTime(DateUtils.formatTime(now));
        return scannerReport;
    }

    public static String getSHA1(String input) {
        try {
            // 1. 获取 SHA-1 算法的 MessageDigest 实例
            MessageDigest digest = MessageDigest.getInstance("SHA-1");

            // 2. 将字符串转换为字节数组（使用 UTF-8 编码）
            byte[] bytes = input.getBytes("UTF-8");

            // 3. 计算哈希值
            byte[] hash = digest.digest(bytes);

            // 4. 将字节数组转换为十六进制字符串
            StringBuilder hexString = new StringBuilder();
            for (byte b : hash) {
                String hex = String.format("%02x", b & 0xFF); // 保证两位十六进制表示
                hexString.append(hex);
            }
            return hexString.toString();

        } catch (Exception e) {
            throw new RuntimeException("SHA-1 计算失败", e);
        }
    }

    public  Artifact  getArtifact(String bomId) throws IOException {

        String regex = "^(?<storageId>[^:]+):(?<repositoryId>[^:]+):(?<path>.+)$";
        Pattern pattern = Pattern.compile(regex);
        Matcher matcher = pattern.matcher(bomId);
        String storageId = "", repositoryId = "";
        String path = null;
        if (matcher.matches()) {
            storageId =  matcher.group("storageId");
            repositoryId =  matcher.group("repositoryId");
            path =  matcher.group("path");
        } else {
            throw new RuntimeException("sbom artifact UUID:"+bomId+"is not exist");
        }
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
        return artifactService.findArtifact(repositoryPath, false);
    }
}
