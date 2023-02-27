package com.veadan.folib.controllers;

import com.alibaba.fastjson.JSON;
import com.google.common.collect.Sets;
import com.veadan.folib.configuration.MutableSecurityPolicyConfiguration;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.VulnerabilitiesInfo;
import com.veadan.folib.domain.Vulnerability;
import com.veadan.folib.enums.BlockTypeEnum;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.layout.DockerLayoutProvider;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.storage.repository.RepositoryDto;
import com.veadan.folib.utils.ArtifactControllerHelper;
import com.veadan.folib.utils.ArtifactUtils;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMethod;

import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.Entity;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.io.IOException;
import java.io.InputStream;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.LinkedHashSet;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

public abstract class BaseArtifactController
        extends BaseController {

    @Inject
    protected ArtifactManagementService artifactManagementService;

    @Inject
    private ArtifactRepository artifactRepository;

    @Value("${folib.dependentPushUrl}")
    private String pushUrl;

    @Autowired
    private ProxyRepositoryConnectionPoolConfigurationService clientPool;


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
        boolean supportLayout = ArtifactUtils.layoutSupports(repositoryPath, true);
        if (!supportLayout) {
            return;
        }
        Artifact artifact = repositoryPath.getArtifactEntry();
        if (Objects.nonNull(artifact)) {
            boolean isDockerLayout = DockerLayoutProvider.ALIAS.equals(repositoryPath.getRepository().getLayout());
            Set<Vulnerability> vulnerabilitySet = artifact.getVulnerabilitySet();
            if (isDockerLayout) {
                String manifest = "manifest";
                String path = repositoryPath.toAbsolutePath().toString();
                if (path.contains("sha256") && !path.endsWith(".sha256") && path.contains(manifest)) {
                    String keywords = path.substring(path.lastIndexOf("manifest/") + "manifest/".length());
                    vulnerabilitySet = artifactRepository.fetchVulnerabilitiesByKeywords(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), keywords);
                }
            }
            Set<String> vulnerabilities = vulnerabilitySet.stream().map(Vulnerability::getUuid).collect(Collectors.toSet());
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
                    flag = vulnerabilities.stream().anyMatch(repositoryBlacks::contains);
                    if (!flag) {
                        Set<String> allSet = Sets.newLinkedHashSet(), blackSet;
                        //不在阻断漏洞等级内的漏洞集合，需要过滤黑名单
                        Set<Vulnerability> unIncludeVulnerabilitySet = Sets.newLinkedHashSet();
                        if (CollectionUtils.isNotEmpty(mutableSecurityPolicyConfiguration.getBlockLevels())) {
                            for (Vulnerability vulnerability : vulnerabilitySet) {
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
                    flag = vulnerabilities.stream().anyMatch(item -> repositoryBlacks.contains(item) ||
                            (!repositoryWhites.contains(item) && platformBlacks.contains(item)));
                } else if (BlockTypeEnum.PACKAGE_NAME.getType().equals(mutableSecurityPolicyConfiguration.getBlockType())) {
                    //包名阻断
                    Set<String> packageNames = mutableSecurityPolicyConfiguration.getPackageNames();
                    if (CollectionUtils.isNotEmpty(packageNames)) {
                        flag = packageNames.stream().anyMatch(packageName -> artifact.getArtifactPath().contains(packageName));
                    }
                }
                if (flag) {
                    // todo 推数据给platform
                    pushVulnerabilities(artifact);
                    throw new RuntimeException(artifact.getUuid() + "制品存在漏洞，禁止下载！！！");
                }
            }
        }
    }

    private void pushVulnerabilities(Artifact artifact) {
        try {
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyyMMdd-HH:mm:ss.SSS");
            String id = LocalDateTime.now().format(formatter) + "-" + UUID.randomUUID().toString();
            String bugName = JSON.toJSONString(artifact.getVulnerabilitySet().stream().
                    map(Vulnerability::getVulnerabilitySource).collect(Collectors.toList()));
            String repairVersion = JSON.toJSONString(artifact.getVulnerabilitySet().stream().
                    map(Vulnerability::getVersionEndExcluding).collect(Collectors.toList()));
            VulnerabilitiesInfo vulnerabilitiesInfo =
                    VulnerabilitiesInfo.builder()
                            .id(id)
                            .appId(artifact.getStorageId())
                            .bugName(bugName)
                            .insertTime(LocalDateTime.now())
                            .packageName(artifact.getRepositoryId())
                            .packagePath(artifact.getArtifactPath())
                            .repairVersion(repairVersion)
                            .report(artifact.getReport()).build();
            Client client = clientPool.getRestClient();
            String url = pushUrl + "/devopsplatform/apis/v1/folib/pushVulnerabilities";
            WebTarget target = client.target(url);
            Response response = target.request().post(Entity.entity(vulnerabilitiesInfo, MediaType.APPLICATION_JSON));
            if (response.getStatus() != 200) {
                throw new Exception("{} get error" + url);
            }
            logger.info("已成功推送漏洞阻断数据");
        } catch (Exception e) {
            logger.error("依赖库漏洞阻断推数据失败");
            e.printStackTrace();
        }
    }

}
