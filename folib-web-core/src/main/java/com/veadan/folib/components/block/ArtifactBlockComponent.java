package com.veadan.folib.components.block;

import com.google.common.collect.Lists;
import com.veadan.folib.artifact.coordinates.DockerArtifactCoordinates;
import com.veadan.folib.components.DistributedCacheComponent;
import com.veadan.folib.configuration.SecurityPolicyConfiguration;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.Vulnerability;
import com.veadan.folib.domain.blockstrategy.BlockStrategyRecord;
import com.veadan.folib.entity.BlockStrategyInfo;
import com.veadan.folib.entity.License;
import com.veadan.folib.entity.PackageNameBlock;
import com.veadan.folib.enums.ConditionTypeEnum;
import com.veadan.folib.enums.SafeLevelEnum;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.io.RootRepositoryPath;
import com.veadan.folib.providers.layout.DockerLayoutProvider;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.scanner.entity.ScanRules;
import com.veadan.folib.scanner.mapper.ScanRulesMapper;
import com.veadan.folib.services.BlockStrategyService;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.services.LicenseService;
import com.veadan.folib.services.PackageNameBlockService;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.utils.VersionUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;
import tk.mybatis.mapper.entity.Example;

import java.util.*;
import java.util.stream.Collectors;

/**
 * @author leipenghui
 * @date 2024/10/28
 **/
@Slf4j
@Component
public class ArtifactBlockComponent {

    @Autowired
    @Lazy
    protected RepositoryPathResolver repositoryPathResolver;

    @Autowired
    @Lazy
    private ArtifactRepository artifactRepository;

    @Autowired
    @Lazy
    private ConfigurationManagementService configurationManagementService;

    @Autowired
    @Lazy
    private BlockStrategyService blockStrategyService;

    @Autowired
    @Lazy
    private PackageNameBlockService packageNameBlockService;

    @Autowired
    @Lazy
    private LicenseService licenseService;

    @Autowired
    private ScanRulesMapper scanRulesMapper;

    @Autowired
    private DistributedCacheComponent distributedCacheComponent;

    /**
     * 判断是否需要阻断
     *
     * @param artifact 制品
     * @param layout   layout
     * @return true
     */
    public boolean artifactBlockStrategy(Artifact artifact, String layout) {
        if (Objects.isNull(artifact)) {
            return false;
        }
        try {
            String storageId = artifact.getStorageId(), repositoryId = artifact.getRepositoryId();
            if (noScanResultBlock(artifact)) {
                return true;
            }
            if (StringUtils.isBlank(layout)) {
                RootRepositoryPath rootRepositoryPath = repositoryPathResolver.resolve(storageId, repositoryId);
                layout = rootRepositoryPath.getRepository().getLayout();
            }
            boolean isDockerLayout = DockerLayoutProvider.ALIAS.equals(layout);
            Set<Vulnerability> vulnerabilitySet = artifact.getVulnerabilitySet();
            if (isDockerLayout) {
                String manifest = "manifest";
                String path = artifact.getUuid();
                if (DockerArtifactCoordinates.include(path) && path.contains(manifest)) {
                    String keywords = path.substring(path.lastIndexOf("manifest/") + "manifest/".length());
                    vulnerabilitySet = artifactRepository.fetchVulnerabilitiesByKeywords(storageId, repositoryId, keywords);
                }
            }
            return blockStrategy(artifact, vulnerabilitySet);
        } catch (Exception ex) {
            log.warn("判断制品 [{}] [{}] [{}] 是否需要阻断错误 [{}]", artifact.getStorageId(), artifact.getRepositoryId(), artifact.getArtifactPath(), ExceptionUtils.getStackTrace(ex));
        }
        return false;
    }

    /**
     * 阻断策略
     *
     * @param artifact         制品
     * @param vulnerabilitySet 制品存在的漏洞
     * @return true 阻断 false 不阻断
     */
    private boolean blockStrategy(Artifact artifact, Set<Vulnerability> vulnerabilitySet) {
        List<BlockStrategyRecord> blockStrategyRecordList = blockStrategyService.getBlockStrategyRecordCache(artifact.getStorageId(), artifact.getRepositoryId());
        if (CollectionUtils.isEmpty(blockStrategyRecordList)) {
            return false;
        }
        for (BlockStrategyRecord blockStrategyRecord : blockStrategyRecordList) {
            try {
                List<String> vulnerabilityLevels = Lists.newArrayList();
                boolean filterVulnerabilityWhites = false, filterVulnerabilityBlacks = false, filterLicenseWhites = false, filterLicenseBlacks = false, filterAllPackageName = false;
                List<String> licenseIds = Lists.newArrayList(), packageNames = Lists.newArrayList();
                if (Objects.nonNull(blockStrategyRecord.getFilterVulnerabilityWhites()) && Boolean.TRUE.equals(blockStrategyRecord.getFilterVulnerabilityWhites())) {
                    filterVulnerabilityWhites = true;
                }
                if (Objects.nonNull(blockStrategyRecord.getFilterVulnerabilityBlacks()) && Boolean.TRUE.equals(blockStrategyRecord.getFilterVulnerabilityBlacks())) {
                    filterVulnerabilityBlacks = true;
                }
                if (Objects.nonNull(blockStrategyRecord.getFilterLicenseWhites()) && Boolean.TRUE.equals(blockStrategyRecord.getFilterLicenseWhites())) {
                    filterLicenseWhites = true;
                }
                if (Objects.nonNull(blockStrategyRecord.getFilterLicenseBlacks()) && Boolean.TRUE.equals(blockStrategyRecord.getFilterLicenseBlacks())) {
                    filterLicenseBlacks = true;
                }
                if (Objects.nonNull(blockStrategyRecord.getFilterAllPackageName()) && Boolean.TRUE.equals(blockStrategyRecord.getFilterAllPackageName())) {
                    filterAllPackageName = true;
                }
                if (StringUtils.isNotBlank(blockStrategyRecord.getVulnerabilityLevels())) {
                    for (String vulnerabilityLevel : blockStrategyRecord.getVulnerabilityLevels().split(GlobalConstants.COMMA)) {
                        if (!vulnerabilityLevels.contains(vulnerabilityLevel)) {
                            vulnerabilityLevels.add(vulnerabilityLevel);
                        }
                    }
                }
                if (CollectionUtils.isNotEmpty(blockStrategyRecord.getBlockStrategyInfos())) {
                    for (BlockStrategyInfo blockStrategyInfo : blockStrategyRecord.getBlockStrategyInfos()) {
                        if (Objects.nonNull(blockStrategyInfo) && StringUtils.isNotBlank(blockStrategyInfo.getPackageName()) && !packageNames.contains(blockStrategyInfo.getPackageName())) {
                            packageNames.add(blockStrategyInfo.getPackageName());
                        }
                        if (Objects.nonNull(blockStrategyInfo) && StringUtils.isNotBlank(blockStrategyInfo.getLicenseId()) && !licenseIds.contains(blockStrategyInfo.getLicenseId())) {
                            licenseIds.add(blockStrategyInfo.getLicenseId());
                        }
                    }
                }
                if (licenseBlock(artifact, filterLicenseBlacks, filterLicenseWhites, licenseIds)) {
                    return true;
                }
                if (packageNameBlock(artifact, filterAllPackageName, packageNames)) {
                    return true;
                }
                if (vulnerabilityBlock(artifact, filterVulnerabilityBlacks, filterVulnerabilityWhites, vulnerabilitySet, vulnerabilityLevels)) {
                    return true;
                }
            } catch (Exception ex) {
                log.error("Artifact [{}] block strategy name [{}] error [{}]", artifact.getUuid(), blockStrategyRecord.getBlockStrategyName(), ExceptionUtils.getStackTrace(ex));
            }
        }
        return false;
    }

    /**
     * 漏洞阻断
     *
     * @param artifact                  制品
     * @param filterVulnerabilityBlacks 是否过滤漏洞白名单
     * @param filterVulnerabilityWhites 是否过滤漏洞黑名单
     * @param vulnerabilitySet          制品的漏洞列表
     * @param vulnerabilityLevels       要阻断的漏洞等级
     * @return true 阻断 false 不阻断
     */
    private boolean vulnerabilityBlock(Artifact artifact, boolean filterVulnerabilityBlacks, boolean filterVulnerabilityWhites, Set<Vulnerability> vulnerabilitySet, List<String> vulnerabilityLevels) {
        if (CollectionUtils.isEmpty(vulnerabilitySet) || (CollectionUtils.isEmpty(vulnerabilityLevels) && !filterVulnerabilityBlacks)) {
            return false;
        }
        Set<String> vulnerabilities = Optional.of(vulnerabilitySet).orElse(Collections.emptySet()).stream().map(Vulnerability::getUuid).collect(Collectors.toSet());
        if (CollectionUtils.isEmpty(vulnerabilities)) {
            return false;
        }
        final Repository repositoryDto = configurationManagementService.getConfiguration().getStorage(artifact.getStorageId()).getRepository(artifact.getRepositoryId());
        Set<String> repositoryBlacks = repositoryDto.getVulnerabilityBlacks();
        Set<String> repositoryWhites = repositoryDto.getVulnerabilityWhites();
        final SecurityPolicyConfiguration mutableSecurityPolicyConfiguration = configurationManagementService.getConfiguration().getSecurityPolicyConfiguration();
        Set<String> platformBlacks = mutableSecurityPolicyConfiguration.getBlacks();
        Set<String> platformWhites = mutableSecurityPolicyConfiguration.getWhites();
        //是否存在仓库级别漏洞黑名单
        if (filterVulnerabilityBlacks && vulnerabilities.stream().anyMatch(repositoryBlacks::contains)) {
            log.warn("Artifact [{}] there is a repository level blacklist", artifact.getUuid());
            return true;
        }
        //是否存在平台级别漏洞黑名单
        if (filterVulnerabilityBlacks && vulnerabilities.stream().anyMatch(platformBlacks::contains)) {
            log.warn("Artifact [{}] there is a platform level blacklist", artifact.getUuid());
            return true;
        }
        for (Vulnerability vulnerability : vulnerabilitySet) {
            //开启漏洞白名单过滤
            if (filterVulnerabilityWhites) {
                //过滤仓库级别白名单、平台级别白名单
                if (repositoryWhites.contains(vulnerability.getUuid()) || platformWhites.contains(vulnerability.getUuid())) {
                    continue;
                }
            }
            if (vulnerabilityLevels.contains(vulnerability.getHighestSeverityText())) {
                log.warn("Artifact [{}] there are blocked vulnerability level vulnerabilities present [{}] [{}]", artifact.getUuid(), vulnerability.getHighestSeverityText(), vulnerability.getUuid());
                return true;
            }
        }
        return false;
    }

    /**
     * 包名阻断
     *
     * @param artifact             制品
     * @param filterAllPackageName 是否过滤全量包名黑名单
     * @param packageNames         选择的包名黑名单
     * @return true 阻断 false 不阻断
     */
    private boolean packageNameBlock(Artifact artifact, boolean filterAllPackageName, List<String> packageNames) {
        if (!filterAllPackageName && CollectionUtils.isEmpty(packageNames)) {
            return false;
        }
        List<PackageNameBlock> packageNameBlockList = packageNameBlockService.getPackageNameBlockCache();
        if (CollectionUtils.isNotEmpty(packageNameBlockList)) {
            if (filterAllPackageName) {
                //全量包名黑名单阻断
                packageNameBlockList = packageNameBlockList.stream().filter(item -> artifact.getArtifactName().contains(item.getPackageName())).collect(Collectors.toList());
                if (CollectionUtils.isEmpty(packageNameBlockList)) {
                    return false;
                }
            } else {
                //选择的包名黑名单
                packageNameBlockList = packageNameBlockList.stream().filter(item -> artifact.getArtifactName().contains(item.getPackageName())).filter(item -> packageNames.contains(item.getPackageName())).collect(Collectors.toList());
                if (CollectionUtils.isEmpty(packageNameBlockList)) {
                    return false;
                }
            }
            boolean block = packageNameBlockList.stream().anyMatch(packageNameBlock -> {
                if (ConditionTypeEnum.RANGE.getCondition().equals(packageNameBlock.getConditionValue())) {
                    String artifactVersion = artifact.getArtifactCoordinates().getVersion();
                    if (StringUtils.isBlank(artifactVersion)) {
                        return false;
                    }
                    long startTime = System.currentTimeMillis();
                    boolean flag = VersionUtils.versionInRange(artifactVersion, packageNameBlock.getVersion());
                    long endTime = System.currentTimeMillis();
                    log.debug("Artifact [{}] Comparing versions takes time [{}] ms", artifact.getUuid(), endTime - startTime);
                    return flag;
                } else if (ConditionTypeEnum.EQ.getCondition().equals(packageNameBlock.getConditionValue())) {
                    String artifactVersion = artifact.getArtifactCoordinates().getVersion();
                    if (StringUtils.isBlank(artifactVersion)) {
                        return false;
                    }
                    return artifact.getArtifactName().contains(packageNameBlock.getPackageName()) && artifactVersion.equals(packageNameBlock.getVersion());
                }
                return artifact.getArtifactName().contains(packageNameBlock.getPackageName());
            });
            if (block) {
                log.warn("Artifact [{}] there is a blacklist of package names", artifact.getUuid());
                return true;
            }
        }
        return false;
    }

    /**
     * license阻断
     *
     * @param artifact            制品
     * @param filterLicenseBlacks 是否过滤license白名单
     * @param filterLicenseWhites 是否过滤license黑名单
     * @param licenseIds          要阻断的license列表
     * @return true 阻断 false 不阻断
     */
    private boolean licenseBlock(Artifact artifact, boolean filterLicenseBlacks, boolean filterLicenseWhites, List<String> licenseIds) {
        if (CollectionUtils.isEmpty(artifact.getComponentSet()) || (!filterLicenseBlacks && CollectionUtils.isEmpty(licenseIds))) {
            return false;
        }
        //license库
        List<License> licenses = licenseService.getLicenseCache();
        Integer white = 1, black = 2;
        //license黑、白名单
        List<String> whiteLicenses = Lists.newArrayList(), blackLicenses = Lists.newArrayList();
        licenses.forEach(license -> {
            if (white.equals(license.getBlackWhiteType())) {
                whiteLicenses.add(license.getLicenseId());
            } else if (black.equals(license.getBlackWhiteType())) {
                blackLicenses.add(license.getLicenseId());
            }
        });
        //制品的license
        for (com.veadan.folib.domain.Component component : artifact.getComponentSet()) {
            if (CollectionUtils.isNotEmpty(component.getLicense())) {
                for (String license : component.getLicense()) {
                    //license黒名单
                    if (filterLicenseBlacks && blackLicenses.contains(license)) {
                        log.warn("Artifact [{}] there is a blacklist of license [{}]", artifact.getUuid(), license);
                        return true;
                    }
                    //license白名单
                    if (filterLicenseWhites && whiteLicenses.contains(license)) {
                        continue;
                    }
                    if (licenseIds.contains(license)) {
                        log.warn("Artifact [{}] there are blocked licenses present [{}]", artifact.getUuid(), license);
                        return true;
                    }
                }
            }
        }
        return false;
    }

    private boolean noScanResultBlock(Artifact artifact) {
        if (enableNoScanResultBlock()) {
            String key = String.format("%s-%s", artifact.getStorageId(), artifact.getRepositoryId());
            String cacheKey = String.format(GlobalConstants.SCAN_ENABLE_REPOSITORY_KEY, key.toUpperCase());
            String cacheValue = distributedCacheComponent.get(cacheKey);
            int count = 0;
            if (StringUtils.isBlank(cacheValue)) {
                Example example = new Example(ScanRules.class);
                example.createCriteria().andEqualTo("id", key)
                        .andEqualTo("onScan", 1);
                count = scanRulesMapper.selectCountByExample(example);
                distributedCacheComponent.put(cacheKey, count + "");
            } else {
                count = Integer.parseInt(cacheValue);
            }
            if (count <= 0) {
                return false;
            }
            List<String> typeList = Lists.newArrayList(SafeLevelEnum.UNWANTED_SCAN.getLevel(), SafeLevelEnum.SCAN_COMPLETE.getLevel());
            if (!typeList.contains(artifact.getSafeLevel())) {
                log.warn("Artifact storageId [{}] repositoryId [{}] artifactPath [{}] safeLevel [{}] no scan result block", artifact.getStorageId(), artifact.getRepositoryId(), artifact.getArtifactPath(), artifact.getSafeLevel());
                return true;
            }
        }
        return false;
    }

    private boolean enableNoScanResultBlock() {
        boolean block = false;
        String cacheValue = distributedCacheComponent.get("ENABLE_NO_SCAN_RESULT_BLOCK");
        if (StringUtils.isNotBlank(cacheValue)) {
            block = Boolean.parseBoolean(cacheValue);
        }
        return block;
    }

}
