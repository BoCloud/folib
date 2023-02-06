package com.veadan.folib.controllers;

import com.google.common.collect.Sets;
import com.veadan.folib.configuration.MutableSecurityPolicyConfiguration;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.Vulnerability;
import com.veadan.folib.enums.BlockTypeEnum;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.storage.repository.RepositoryDto;
import com.veadan.folib.utils.ArtifactControllerHelper;
import com.veadan.folib.utils.ArtifactUtils;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMethod;

import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.InputStream;
import java.util.LinkedHashSet;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

public abstract class BaseArtifactController
        extends BaseController {

    @Inject
    protected ArtifactManagementService artifactManagementService;

    protected boolean provideArtifactDownloadResponse(HttpServletRequest request,
                                                      HttpServletResponse response,
                                                      HttpHeaders httpHeaders,
                                                      RepositoryPath repositoryPath)
            throws Exception {
        logger.debug("Resolved path: {}", repositoryPath);
        ArtifactControllerHelper.provideArtifactHeaders(response, repositoryPath);
        if (response.getStatus() == HttpStatus.NOT_FOUND.value()) {
            return false;
        } else if (request.getMethod().equals(RequestMethod.HEAD.name())) {
            return true;
        }


        try (InputStream is = artifactResolutionService.getInputStream(repositoryPath)) {
            if (ArtifactControllerHelper.isRangedRequest(httpHeaders)) {
                logger.debug("Detected ranged request.");

                ArtifactControllerHelper.handlePartialDownload(is, httpHeaders, response);
            } else {
                copyToResponse(is, response);
            }
        }

        return true;
    }

    public ResponseEntity<String> checkRepositoryAccess() {
        return new ResponseEntity<>("success", HttpStatus.OK);
    }

    /**
     * 漏洞阻断下载
     *
     * @param repositoryPath 制品信息
     * @throws IOException io异常
     */
    public void vulnerabilityBlock(RepositoryPath repositoryPath) throws IOException {
        boolean supportLayout = ArtifactUtils.layoutSupports(repositoryPath);
        if (!supportLayout) {
            return;
        }
        Artifact artifact = repositoryPath.getArtifactEntry();
        if (Objects.nonNull(artifact)) {
            MutableSecurityPolicyConfiguration mutableSecurityPolicyConfiguration = configurationManagementService.getMutableConfigurationClone().getSecurityPolicyConfiguration();
            if (Objects.nonNull(mutableSecurityPolicyConfiguration)) {
                RepositoryDto repositoryDto = configurationManagementService.getMutableConfigurationClone().getStorage(repositoryPath.getStorageId()).getRepository(repositoryPath.getRepositoryId());
                Set<String> repositoryBlacks = repositoryDto.getVulnerabilityBlacks();
                Set<String> repositoryWhites = repositoryDto.getVulnerabilityWhites();
                Set<String> platformBlacks = mutableSecurityPolicyConfiguration.getBlacks();
                Set<String> platformWhites = mutableSecurityPolicyConfiguration.getWhites();
                boolean flag = false;
                if (BlockTypeEnum.ALL.getType().equals(mutableSecurityPolicyConfiguration.getBlockType())) {
                    //过滤仓库级别黑名单
                    flag = artifact.getVulnerabilities().stream().anyMatch(repositoryBlacks::contains);
                    if (!flag) {
                        Set<String> allSet = Sets.newLinkedHashSet(), blackSet;
                        //不在阻断漏洞等级内的漏洞集合，需要过滤黑名单
                        Set<Vulnerability> unIncludeVulnerabilitySet = Sets.newLinkedHashSet();
                        if (CollectionUtils.isNotEmpty(mutableSecurityPolicyConfiguration.getBlockLevels())) {
                            for (Vulnerability vulnerability : artifact.getVulnerabilitySet()) {
                                //开启白名单过滤
                                if (Boolean.TRUE.equals(mutableSecurityPolicyConfiguration.getFilterWhites())) {
                                    //过滤仓库级别白名单、平台级别白名单
                                    if (repositoryWhites.contains(vulnerability.getUuid()) || platformWhites.contains(vulnerability.getUuid())) {
                                        continue;
                                    }
                                }
                                if (mutableSecurityPolicyConfiguration.getBlockLevels().contains(vulnerability.getHighestSeverityText())) {
                                    allSet.add(vulnerability.getUuid());
                                } else {
                                    unIncludeVulnerabilitySet.add(vulnerability);
                                }
                            }
                        }
                        //过滤平台级别黑名单
                        blackSet = unIncludeVulnerabilitySet.stream().filter(item -> platformBlacks.contains(item.getUuid())).map(Vulnerability::getUuid).collect(Collectors.toCollection(LinkedHashSet::new));
                        allSet.addAll(blackSet);
                        flag = CollectionUtils.isNotEmpty(allSet);
                    }
                } else if (BlockTypeEnum.BLACK.getType().equals(mutableSecurityPolicyConfiguration.getBlockType())) {
                    //黑名单阻断
                    flag = artifact.getVulnerabilities().stream().anyMatch(item -> repositoryBlacks.contains(item) ||
                            (!repositoryWhites.contains(item) && platformBlacks.contains(item)));
                } else if (BlockTypeEnum.PACKAGE_NAME.getType().equals(mutableSecurityPolicyConfiguration.getBlockType())) {
                    //包名阻断
                    Set<String> packageNames = mutableSecurityPolicyConfiguration.getPackageNames();
                    if (CollectionUtils.isNotEmpty(packageNames)) {
                        flag = packageNames.stream().anyMatch(packageName -> artifact.getArtifactPath().contains(packageName));
                    }
                }
                if (flag) {
                    throw new RuntimeException(artifact.getUuid() + "制品存在漏洞，禁止下载！！！");
                }
            }
        }
    }

}
