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
package com.folib.services.impl;

import cn.hutool.core.collection.CollectionUtil;
import cn.hutool.core.date.DatePattern;
import cn.hutool.core.date.DateUtil;
import cn.hutool.core.io.FileUtil;
import cn.hutool.core.io.IORuntimeException;
import cn.hutool.core.lang.UUID;
import cn.hutool.json.JSONUtil;
import com.alibaba.excel.EasyExcel;
import com.alibaba.excel.ExcelWriter;
import com.alibaba.excel.write.metadata.WriteSheet;
import com.alibaba.excel.write.metadata.fill.FillConfig;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.alibaba.fastjson.parser.Feature;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.xml.XmlMapper;
import com.folib.domain.*;
import com.folib.enums.ArtifactMetadataEnum;
import com.folib.enums.ProductTypeEnum;
import com.folib.enums.RepositoryScopeEnum;
import com.folib.forms.scanner.*;
import com.folib.services.*;
import com.folib.util.*;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.sun.management.HotSpotDiagnosticMXBean;
import com.folib.artifact.coordinates.DockerCoordinates;
import com.folib.artifact.coordinates.MavenCoordinates;
import com.folib.authorization.dto.Role;
import com.folib.components.DistributedLockComponent;
import com.folib.components.artifact.ArtifactComponent;
import com.folib.components.auth.AuthComponent;
import com.folib.components.layout.DockerComponent;
import com.folib.components.thirdparty.foeyes.FoEyesComponent;
import com.folib.components.thirdparty.foeyes.enums.UploadStatusEnum;
import com.folib.configuration.ConfigurationManager;
import com.folib.configuration.ConfigurationUtils;
import com.folib.configuration.MutableMetadataConfiguration;
import com.folib.constant.DebianConstant;
import com.folib.constant.GlobalConstants;
import com.folib.controllers.ResponseMessage;
import com.folib.domain.bom.Bom;
import com.folib.domain.bom.FoEyes;
import com.folib.domain.thirdparty.ArtifactInfo;
import com.folib.domain.thirdparty.ArtifactQuery;
import com.folib.entity.Dict;
import com.folib.entity.RoleResourceRef;
import com.folib.event.artifact.ArtifactEventListenerRegistry;
import com.folib.forms.artifact.ArtifactMetadataForm;
import com.folib.gremlin.dsl.EntityTraversalUtils;
import com.folib.gremlin.entity.vo.ArtifactVo;
import com.folib.indexer.DebianReleaseMetadataIndexer;
import com.folib.mapper.RoleResourceRefMapper;
import com.folib.metadata.indexer.RpmRepoIndexer;
import com.folib.promotion.PromotionUtil;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.providers.io.RootRepositoryPath;
import com.folib.providers.layout.DebianLayoutProvider;
import com.folib.providers.layout.DockerLayoutProvider;
import com.folib.providers.layout.LayoutProvider;
import com.folib.providers.layout.LayoutProviderRegistry;
import com.folib.providers.layout.Maven2LayoutProvider;
import com.folib.repositories.ArtifactRepository;
import com.folib.scanner.common.exception.BusinessException;
import com.folib.scanner.common.msg.TableResultResponse;
import com.folib.scanner.entity.ScanRules;
import com.folib.scanner.mapper.ScanRulesMapper;
import com.folib.scanner.service.SbomScannerService;
import com.folib.schema2.ContainerConfigurationManifest;
import com.folib.schema2.ImageManifest;
import com.folib.schema2.LayerManifest;
import com.folib.schema2.Manifests;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryTypeEnum;
import com.folib.users.domain.Privileges;
import com.folib.users.domain.SystemRole;
import com.folib.users.userdetails.SpringSecurityUser;
import com.folib.aql.utils.TreeUtil;
import jakarta.transaction.Transactional;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.util.UriComponentsBuilder;

import javax.inject.Inject;
import jakarta.servlet.http.HttpServletResponse;
import java.io.*;
import java.lang.management.ManagementFactory;
import java.math.BigDecimal;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.concurrent.FutureTask;
import java.util.concurrent.atomic.AtomicLong;
import java.util.regex.Matcher;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Slf4j
@Service
@Transactional
public class ArtifactWebServiceImpl implements ArtifactWebService {

    @Inject
    @Lazy
    private ArtifactRepository artifactRepository;

    @Inject
    @Lazy
    private ArtifactService artifactService;

    @Inject
    @Lazy
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    @Lazy
    private ConfigurationManagementService configurationManagementService;

    @Inject
    @Lazy
    private ScanRulesMapper scanRulesMapper;

    @Inject
    @Lazy
    private DistributedLockComponent distributedLockComponent;

    @Inject
    @Qualifier("browseRepositoryDirectoryListingService")
    private DirectoryListingService directoryListingService;

    @Autowired
    @Lazy
    private ArtifactEventListenerRegistry artifactEvent;

    @Inject
    @Lazy
    private ArtifactManagementService artifactManagementService;

    @Inject
    @Lazy
    private ThreadPoolTaskExecutor asyncThreadPoolTaskExecutor;

    @Inject
    @Lazy
    private SbomScannerService scanService;

    @Inject
    @Lazy
    private DictService dictService;

    @Inject
    private ArtifactComponent artifactComponent;

    @Inject
    private ArtifactMetadataService artifactMetadataService;

    @Inject
    @Lazy
    private PromotionUtil promotionUtil;

    @Inject
    private LayoutProviderRegistry layoutProviderRegistry;

    @Inject
    @Lazy
    private DockerComponent dockerComponent;

    @Inject
    private FoEyesComponent foEyesComponent;

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private StorageManagementService storageManagementService;

    @Autowired
    @Lazy
    private AuthComponent authComponent;

    @Autowired
    private RoleResourceRefMapper roleResourceRefMapper;

    @Value("${folib.temp}")
    private String tempPath;

    @Override
    public void exportExcel(String vulnerabilityUuid, String storageId, String repositoryId, HttpServletResponse response) throws IOException {
        List<String> storageIdAndRepositoryIdList = getStorageIdAndRepositoryId(storageId, repositoryId);
        List<Artifact> artifactList = null;
        long count = artifactRepository.countByVulnerabilityUuid(vulnerabilityUuid, null, storageIdAndRepositoryIdList, "");
        if (count > 0) {
            Page<Artifact> artifactPage = artifactRepository.findMatchingByVulnerabilityUuid(PageRequest.of(1, (int) count).first(), vulnerabilityUuid, null, storageIdAndRepositoryIdList, "");
            if (Objects.nonNull(artifactPage) && CollectionUtils.isNotEmpty(artifactPage.getContent())) {
                artifactList = artifactPage.getContent();
            }
        }
        InputStream template = this.getClass().getResourceAsStream("/template/vulnerabilityTemplate.xlsx");
        try (ExcelWriter excelWriter = EasyExcel.write(response.getOutputStream()).withTemplate(template).build()) {
            WriteSheet writeSheet = EasyExcel.writerSheet().build();
            FillConfig fillConfig = FillConfig.builder().build();
            Map<String, Object> map = Maps.newHashMap();
            map.put("vulnerabilityID", vulnerabilityUuid);
            excelWriter.fill(map, writeSheet);
            if (CollectionUtils.isNotEmpty(artifactList)) {
                SimpleDateFormat df = DateUtil.newSimpleFormat("yyyy-MM-dd HH:mm:ss");
                List<List<Artifact>> list = Lists.partition(artifactList, 200);
                for (List<Artifact> itemList : list) {
                    // 放入数据
                    excelWriter.fill(itemList.stream().map(artifact -> {
                        ArtifactVo artifactVo = ArtifactVo.builder().build();
                        BeanUtils.copyProperties(artifact, artifactVo);
                        if (artifact.getCreated() != null) {
                            String createdTime = DateUtil.format(Date.from(artifact.getCreated().atZone(ZoneId.of("Asia/Shanghai")).toOffsetDateTime().toInstant()), df);
                            artifactVo.setCreatedTime(createdTime);
                        }
                        if (artifact.getLastUsed() != null) {
                            String lastUsedTime = DateUtil.format(Date.from(artifact.getLastUsed().atZone(ZoneId.of("Asia/Shanghai")).toOffsetDateTime().toInstant()), df);
                            artifactVo.setLastUsedTime(lastUsedTime);
                        }
                        artifactVo.setSha(artifact.getChecksums().get("SHA-1"));
                        artifactVo.setMd5(artifact.getChecksums().get("MD5"));
                        artifactVo.setSize(FileSizeConvertUtils.convert(artifact.getSizeInBytes()));
                        artifactVo.setName(artifact.getUuid().substring(artifact.getUuid().lastIndexOf("/") + 1));
                        if (StringUtils.isNotBlank(artifact.getStorageId()) && StringUtils.isNotBlank(artifact.getRepositoryId())) {
                            Repository repository = configurationManagementService.getConfiguration().getRepository(artifact.getStorageId(), artifact.getRepositoryId());
                            if (Objects.nonNull(repository) && "Docker".equalsIgnoreCase(repository.getLayout())) {
                                //docker
                                artifactVo.setName(artifactComponent.getDockerImage(artifact.getArtifactPath()));
                            }
                        }
                        return artifactVo;
                    }).collect(Collectors.toList()), fillConfig, writeSheet);
                }
            }
            // 设置响应头
            response.setContentType("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet");
            response.setCharacterEncoding("utf-8");
            // encode可以防止中文乱码
            String fileName = UriUtils.encode(vulnerabilityUuid + "影响范围");
            response.setHeader("Content-disposition", "attachment;filename*=utf-8''" + fileName + ".xlsx");
            excelWriter.finish();
        }
    }

    @Override
    public TableResultResponse<com.folib.domain.ArtifactInfo> getArtifacts(Integer pageNumber, Integer pageSize, String vulnerabilityUuid, String storageId, String repositoryId, String artifactName) {
        Pageable pageable;
        Integer page = pageNumber, limit = pageSize;
        if (Objects.isNull(page)) {
            page = 1;
        }
        if (Objects.isNull(limit)) {
            limit = 10;
        }
        if (page == 1) {
            pageable = PageRequest.of(page, limit).first();
        } else {
            pageable = PageRequest.of(page, limit).previous();
        }
        List<String> storageIdAndRepositoryIdList = getStorageIdAndRepositoryId(storageId, repositoryId);
        List<com.folib.domain.ArtifactInfo> artifactInfoList = null;
        TableResultResponse<com.folib.domain.ArtifactInfo> tableResultResponse = new TableResultResponse<>(0, null);
        Page<Artifact> artifactPage = artifactRepository.findMatchingByVulnerabilityUuid(pageable, vulnerabilityUuid, null, storageIdAndRepositoryIdList, artifactName);
        if (Objects.nonNull(artifactPage) && CollectionUtils.isNotEmpty(artifactPage.getContent())) {
            String locationKey = "location", tableKey = "table", valueKey = "value";
            artifactInfoList = artifactPage.getContent().stream().map(artifact -> {
                com.folib.domain.ArtifactInfo artifactInfo = new com.folib.domain.ArtifactInfo();
                BeanUtils.copyProperties(artifact, artifactInfo);
                try {
                    RepositoryPath repositoryPath = repositoryPathResolver.resolve(artifact.getStorageId(),
                            artifact.getRepositoryId(),
                            artifact.getArtifactPath());
                    Repository repository = repositoryPath.getRepository();
                    if (DockerLayoutProvider.ALIAS.equalsIgnoreCase(repository.getLayout())) {
                        DockerCoordinates dockerArtifactCoordinates = (DockerCoordinates) artifact.getArtifactCoordinates();
                        artifactInfo.setArtifactPath(dockerArtifactCoordinates.getIMAGE_NAME().replace(":", "/"));
                    }
                } catch (Exception ex) {
                    log.warn(ExceptionUtils.getStackTrace(ex));
                }
                if (StringUtils.isBlank(artifactInfo.getMetadata()) || !JSONUtil.isJson(artifactInfo.getMetadata())) {
                    return artifactInfo;
                }
                JSONObject metadataJson = JSONObject.parseObject(artifactInfo.getMetadata()), metadataValueJson = null;
                String value = "";
                List<List<ColumnInfo>> columnInfos = Lists.newArrayList();
                List<List<JSONObject>> data = Lists.newArrayList();
                try {
                    for (String metadataKey : metadataJson.keySet()) {
                        value = metadataJson.getString(metadataKey);
                        if (StringUtils.isBlank(value) || !JSONUtil.isJson(value)) {
                            continue;
                        }
                        metadataValueJson = metadataJson.getJSONObject(metadataKey);
                        if (!metadataValueJson.containsKey(valueKey) || !metadataValueJson.containsKey(locationKey) || !tableKey.equalsIgnoreCase(metadataValueJson.getString(locationKey))) {
                            continue;
                        }
                        value = metadataValueJson.getString(valueKey);
                        if (StringUtils.isBlank(value) || !JSONUtil.isJson(value)) {
                            continue;
                        }
                        if (JSONUtil.isJsonArray(value)) {
                            JSONArray jsonArray = (JSONArray) JSONArray.parse(value, Feature.OrderedField);
                            if (Objects.isNull(jsonArray) || jsonArray.isEmpty()) {
                                continue;
                            }
                            JSONObject itemJson = null;
                            List<ColumnInfo> itemColumnInfos = Lists.newArrayList();
                            List<JSONObject> itemData = Lists.newArrayList();
                            if (CollectionUtils.isEmpty(itemColumnInfos)) {
                                value = jsonArray.getString(0);
                                if (StringUtils.isBlank(value) || !JSONUtil.isJson(value)) {
                                    continue;
                                }
                                itemJson = jsonArray.getJSONObject(0);
                                itemColumnInfos = itemJson.keySet().stream().map(item -> ColumnInfo.builder().dataIndex(item).key(item).title(item).build()).collect(Collectors.toList());
                            }
                            for (int i = 0; i < jsonArray.size(); i++) {
                                value = jsonArray.getString(i);
                                if (StringUtils.isBlank(value) || !JSONUtil.isJson(value)) {
                                    continue;
                                }
                                itemJson = jsonArray.getJSONObject(i);
                                itemData.add(itemJson);
                            }
                            columnInfos.add(itemColumnInfos);
                            data.add(itemData);
                        } else {
                            JSONObject valueJson = JSONObject.parseObject(metadataValueJson.getString(valueKey), Feature.OrderedField), itemJson = null;
                            JSONArray jsonArray = null;
                            for (String key : valueJson.keySet()) {
                                value = valueJson.getString(key);
                                if (StringUtils.isBlank(value) || !JSONUtil.isJsonArray(value)) {
                                    continue;
                                }
                                jsonArray = valueJson.getJSONArray(key);
                                if (Objects.isNull(jsonArray) || jsonArray.isEmpty()) {
                                    continue;
                                }
                                List<ColumnInfo> itemColumnInfos = Lists.newArrayList();
                                List<JSONObject> itemData = Lists.newArrayList();
                                if (CollectionUtils.isEmpty(itemColumnInfos)) {
                                    value = jsonArray.getString(0);
                                    if (StringUtils.isBlank(value) || !JSONUtil.isJson(value)) {
                                        continue;
                                    }
                                    itemJson = jsonArray.getJSONObject(0);
                                    itemColumnInfos = itemJson.keySet().stream().map(item -> ColumnInfo.builder().dataIndex(item).key(item).title(item).build()).collect(Collectors.toList());
                                }
                                for (int i = 0; i < jsonArray.size(); i++) {
                                    value = jsonArray.getString(i);
                                    if (StringUtils.isBlank(value) || !JSONUtil.isJson(value)) {
                                        continue;
                                    }
                                    itemJson = jsonArray.getJSONObject(i);
                                    itemData.add(itemJson);
                                }
                                columnInfos.add(itemColumnInfos);
                                data.add(itemData);
                            }
                        }
                    }
                } catch (Exception ex) {
                    log.warn(ExceptionUtils.getStackTrace(ex));
                }
                artifactInfo.setColumns(columnInfos);
                artifactInfo.setData(data);
                return artifactInfo;
            }).collect(Collectors.toList());
            tableResultResponse = new TableResultResponse<>(artifactPage.getTotalElements(), artifactInfoList);
        }
        return tableResultResponse;
    }

    @Override
    public void globalSettingAddOrUpdateMetadata(ArtifactMetadataForm artifactMetadataForm) throws IOException {
        MutableMetadataConfiguration mutableMetadataConfiguration = MutableMetadataConfiguration.builder().build();
        BeanUtils.copyProperties(artifactMetadataForm, mutableMetadataConfiguration);
        configurationManagementService.addOrUpdateMetadataConfiguration(mutableMetadataConfiguration);
    }

    @Override
    public void globalSettingDeleteMetadata(ArtifactMetadataForm artifactMetadataForm) throws IOException {
        configurationManagementService.deleteMetadataConfig(artifactMetadataForm.getKey());
    }

    @Override
    public List<ArtifactMetadataForm> getMetadataConfiguration() {
        return Optional.of(configurationManagementService.getConfiguration().getMetadataConfiguration().values().stream().collect(Collectors.toCollection(LinkedList::new))).orElse(Lists.newLinkedList()).stream().map(item -> {
            ArtifactMetadataForm artifactMetadata = ArtifactMetadataForm.builder().build();
            BeanUtils.copyProperties(item, artifactMetadata);
            return artifactMetadata;
        }).collect(Collectors.toList());
    }

    @Override
    public String saveArtifactMetadata(ArtifactMetadataForm artifactMetadataForm) {
        return updateArtifactMetadata(artifactMetadataForm);
    }

    @Override
    public String updateArtifactMetadata(ArtifactMetadataForm artifactMetadataForm) {
        String lockKey = String.format("%s-%s-%s-%s", "metadata", artifactMetadataForm.getStorageId(), artifactMetadataForm.getRepositoryId(), artifactMetadataForm.getArtifactPath());
        if (distributedLockComponent.lock(lockKey, GlobalConstants.WAIT_LOCK_TIME)) {
            try {
                RepositoryPath repositoryPath = repositoryPathResolver.resolve(artifactMetadataForm.getStorageId(), artifactMetadataForm.getRepositoryId(), artifactMetadataForm.getArtifactPath());
                if (Objects.isNull(repositoryPath) || !Files.exists(repositoryPath)) {
                    throw new BusinessException(String.format(GlobalConstants.ARTIFACT_NOT_FOUND_MESSAGE, artifactMetadataForm.getStorageId(), artifactMetadataForm.getRepositoryId(), artifactMetadataForm.getArtifactPath()));
                }
                validateAuth(repositoryPath);
                boolean isDocker = DockerLayoutProvider.ALIAS.equalsIgnoreCase(repositoryPath.getRepository().getLayout());
                boolean isDockerTag = isDocker && DockerCoordinates.isDockerTag(repositoryPath);
                if (!isDockerTag && Files.isDirectory(repositoryPath)) {
                    recursiveMetadata(repositoryPath, artifactMetadataForm);
                    return ResponseMessage.ok().getMessage();
                } else {
                    Artifact artifact = resolvePath(artifactMetadataForm.getStorageId(), artifactMetadataForm.getRepositoryId(), artifactMetadataForm.getArtifactPath());
                    if (Objects.isNull(artifact)) {
                        throw new BusinessException(String.format(GlobalConstants.ARTIFACT_NOT_FOUND_MESSAGE, artifactMetadataForm.getStorageId(), artifactMetadataForm.getRepositoryId(), artifactMetadataForm.getArtifactPath()));
                    }
                    saveOrUpdateArtifactMetadata(repositoryPath, artifact, artifactMetadataForm);
                }
            } catch (Exception e) {
                log.error("Update artifact metadata error [{}]", ExceptionUtils.getStackTrace(e));
                if (e instanceof BusinessException) {
                    throw new RuntimeException(e.getMessage());
                } else {
                    log.error("修改制品元数据错误：{}", ExceptionUtils.getStackTrace(e));
                    throw new RuntimeException("修改制品元数据错误，请稍后重试");
                }
            } finally {
                distributedLockComponent.unLock(lockKey);
            }
        } else {
            log.warn("Artifact [{}] was not get lock", lockKey);
        }
        return ResponseMessage.ok().getMessage();
    }

    @Override
    public void deleteArtifactMetadata(ArtifactMetadataForm artifactMetadataForm) {
        String lockKey = String.format("%s-%s-%s-%s", "metadata", artifactMetadataForm.getStorageId(), artifactMetadataForm.getRepositoryId(), artifactMetadataForm.getArtifactPath());
        if (distributedLockComponent.lock(lockKey, GlobalConstants.WAIT_LOCK_TIME)) {
            try {
                RepositoryPath repositoryPath = repositoryPathResolver.resolve(artifactMetadataForm.getStorageId(), artifactMetadataForm.getRepositoryId(), artifactMetadataForm.getArtifactPath());
                if (Objects.isNull(repositoryPath) || !Files.exists(repositoryPath)) {
                    throw new BusinessException(String.format(GlobalConstants.ARTIFACT_NOT_FOUND_MESSAGE, artifactMetadataForm.getStorageId(), artifactMetadataForm.getRepositoryId(), artifactMetadataForm.getArtifactPath()));
                }
                validateAuth(repositoryPath);
                boolean isDocker = DockerLayoutProvider.ALIAS.equalsIgnoreCase(repositoryPath.getRepository().getLayout());
                boolean isDockerTag = isDocker && DockerCoordinates.isDockerTag(repositoryPath);
                if (!isDockerTag && Files.isDirectory(repositoryPath)) {
                    recursiveDeleteMetadata(repositoryPath, artifactMetadataForm);
                    return;
                }
                Artifact artifact = resolvePath(artifactMetadataForm.getStorageId(), artifactMetadataForm.getRepositoryId(), artifactMetadataForm.getArtifactPath());
                if (Objects.isNull(artifact)) {
                    throw new BusinessException(String.format(GlobalConstants.ARTIFACT_NOT_FOUND_MESSAGE, artifactMetadataForm.getStorageId(), artifactMetadataForm.getRepositoryId(), artifactMetadataForm.getArtifactPath()));
                }
                deleteArtifactMetadata(repositoryPath, artifact, artifactMetadataForm);
            } catch (Exception e) {
                log.error("Delete artifact metadata error [{}]", ExceptionUtils.getStackTrace(e));
                if (e instanceof BusinessException) {
                    throw new RuntimeException(e.getMessage());
                } else {
                    log.error("删除制品元数据错误：{}", ExceptionUtils.getStackTrace(e));
                    throw new RuntimeException("删除制品元数据错误，请稍后重试");
                }
            } finally {
                distributedLockComponent.unLock(lockKey);
            }
        } else {
            log.warn("Artifact [{}] was not get lock", lockKey);
        }
    }

    @Override
    public CountForm getCount(Authentication authentication) {
        Long zero = 0L;
        CountForm countForm = CountForm.builder().scanCount(zero).notScanCount(zero).scanSuccessCount(zero).scanFailCount(zero)
                .dependencyCount(zero).dependencyVulnerabilitiesCount(zero).vulnerabilitiesCount(zero).suppressedVulnerabilitiesCount(zero).build();
        List<String> storageIds = havePermissionStorageIdList(authentication);
        List<String> storageIdAndRepositoryIdList = getStorageIdsRepositoryIdsByOnScanAndStorageIds(1, storageIds);
        if (CollectionUtils.isEmpty(storageIdAndRepositoryIdList)) {
            return null;
        }
        List<String> disableStorageIdAndRepositoryIdList = getStorageIdsRepositoryIdsByOnScanAndStorageIds(0, storageIds);
        Map<String, Long> map = artifactRepository.countArtifactByStorageIdsAndRepositories(storageIdAndRepositoryIdList, disableStorageIdAndRepositoryIdList);
        countForm.setScanCount(map.getOrDefault("scanCount", zero));
        countForm.setNotScanCount(map.getOrDefault("notScanCount", zero));
        countForm.setScanSuccessCount(map.getOrDefault("scanSuccessCount", zero));
        countForm.setUnScanCount(map.getOrDefault("unScanCount", zero));
        countForm.setScanFailCount(map.getOrDefault("scanFailCount", zero));
        countForm.setDependencyCount(map.getOrDefault("dependencyCount", zero));
        countForm.setDependencyVulnerabilitiesCount(map.getOrDefault("dependencyVulnerabilitiesCount", zero));
        countForm.setVulnerabilitiesCount(map.getOrDefault("vulnerabilitiesCount", zero));
        countForm.setSuppressedVulnerabilitiesCount(map.getOrDefault("suppressedVulnerabilitiesCount", zero));
        return countForm;
    }

    @Override
    public List<DayCountForm> monthCount(Authentication authentication) {
        List<String> storageIds = havePermissionStorageIdList(authentication);
        List<String> dayList = CustomDateUtils.getDaysBetween(30);
        Map<String, Long> map = null;
        List<DayCountForm> list = Lists.newArrayList();
        Long zero = 0L, dependencyCount, vulnerabilitiesCount;
        List<String> storageIdAndRepositoryIdList = getStorageIdsRepositoryIdsByOnScanAndStorageIds(1, storageIds);
        if (CollectionUtils.isEmpty(storageIdAndRepositoryIdList)) {
            return null;
        }
        for (String date : dayList) {
            map = artifactRepository.countArtifactByStorageIdsAndRepositoryIdsAndDate(storageIdAndRepositoryIdList, date, null, null);
            dependencyCount = map.getOrDefault("dependencyCount", zero);
            vulnerabilitiesCount = map.getOrDefault("vulnerabilitiesCount", zero);
            if (dependencyCount > zero || vulnerabilitiesCount > zero) {
                list.add(DayCountForm.builder().date(date).dependencyCount(dependencyCount).vulnerabilitiesCount(vulnerabilitiesCount).build());
            }
        }
        return list;
    }

    @Override
    public WeekCountForm weekCount(Authentication authentication) {
        List<String> storageIds = havePermissionStorageIdList(authentication);
        List<String> currentWeekList = CustomDateUtils.getDaysBetween(7);
        List<String> lastWeekList = CustomDateUtils.getDaysBetween(14);
        lastWeekList.removeAll(currentWeekList);
        Map<String, Long> map = null;
        WeekCountForm weekCountForm = WeekCountForm.builder().build();
        List<WeekDayCountForm> list = Lists.newArrayList();
        Long zero = 0L, vulnerabilitiesCount;
        List<String> storageIdAndRepositoryIdList = getStorageIdsRepositoryIdsByOnScanAndStorageIds(1, storageIds);
        if (CollectionUtils.isEmpty(storageIdAndRepositoryIdList)) {
            return null;
        }
        for (String date : currentWeekList) {
            map = artifactRepository.countArtifactByStorageIdsAndRepositoryIdsAndDate(storageIdAndRepositoryIdList, date, null, null);
            vulnerabilitiesCount = map.getOrDefault("vulnerabilitiesCount", zero);
            list.add(WeekDayCountForm.builder().date(date.substring(5)).vulnerabilitiesCount(vulnerabilitiesCount).build());
        }
        weekCountForm.setDayCountList(list);

        Map<String, Long> currentWeekMap = artifactRepository.countFullArtifactByStorageIdsAndRepositoryIdsAndDate(storageIdAndRepositoryIdList, getStartLong(currentWeekList.get(0)), getEndLong(currentWeekList.get(currentWeekList.size() - 1)));
        Map<String, Long> lastWeekMap = artifactRepository.countFullArtifactByStorageIdsAndRepositoryIdsAndDate(storageIdAndRepositoryIdList, getStartLong(lastWeekList.get(0)), getEndLong(lastWeekList.get(lastWeekList.size() - 1)));
        CompareCountForm compareCountForm = CompareCountForm.builder().build();
        compareCountForm.setScanCount(currentWeekMap.getOrDefault("scanCount", zero) - lastWeekMap.getOrDefault("scanCount", zero));
        compareCountForm.setDependencyCount(currentWeekMap.getOrDefault("dependencyCount", zero) - lastWeekMap.getOrDefault("dependencyCount", zero));
        compareCountForm.setDependencyVulnerabilitiesCount(currentWeekMap.getOrDefault("dependencyVulnerabilitiesCount", zero) - lastWeekMap.getOrDefault("dependencyVulnerabilitiesCount", zero));
        compareCountForm.setVulnerabilitiesCount(currentWeekMap.getOrDefault("vulnerabilitiesCount", zero) - lastWeekMap.getOrDefault("vulnerabilitiesCount", zero));
        compareCountForm.setSuppressedVulnerabilitiesCount(currentWeekMap.getOrDefault("suppressedVulnerabilitiesCount", zero) - lastWeekMap.getOrDefault("suppressedVulnerabilitiesCount", zero));
        weekCountForm.setCompareCount(compareCountForm);
        return weekCountForm;
    }

    @Override
    public List<RepositoryCountForm> repositories(Authentication authentication) {
        List<String> storageIds = havePermissionStorageIdList(authentication);
        if (CollectionUtils.isEmpty(storageIds)) {
            return Collections.emptyList();
        }
        List<String> storageIdAndRepositoryIdList = getStorageIdAndRepositoryIdList(storageIds);
        if (CollectionUtils.isEmpty(storageIdAndRepositoryIdList)) {
            return Collections.emptyList();
        }
        List<ScanRules> scanRulesList = scanRulesMapper.selectList(Wrappers.<ScanRules>lambdaQuery()
                .eq(ScanRules::getOnScan, 1)
                .in(ScanRules::getId, storageIdAndRepositoryIdList));
        Long zero = 0L;
        DecimalFormat decimalFormat = new DecimalFormat(".00");
        return Optional.ofNullable(scanRulesList).orElse(Collections.emptyList()).stream().map(scanRules -> {
            RepositoryCountForm repositoryCountForm = RepositoryCountForm.builder().storage(scanRules.getStorage()).repository(scanRules.getRepository())
                    .layout(scanRules.getLayout()).build();
            Map<String, Long> map = artifactRepository.countRepositoryArtifactByStorageIdAndRepositoryId(scanRules.getStorage(), scanRules.getRepository());
            repositoryCountForm.setScanCount(map.getOrDefault("scanCount", zero));
            repositoryCountForm.setDependencyCount(map.getOrDefault("dependencyCount", zero));
            repositoryCountForm.setDependencyVulnerabilitiesCount(map.getOrDefault("dependencyVulnerabilitiesCount", zero));
            repositoryCountForm.setVulnerabilitiesCount(map.getOrDefault("vulnerabilitiesCount", zero));
            repositoryCountForm.setSuppressedVulnerabilitiesCount(map.getOrDefault("suppressedVulnerabilitiesCount", zero));
            String r;
            if (repositoryCountForm.getVulnerabilitiesCount() == 0) {
                r = "100";
            } else {
                r = decimalFormat.format((float) repositoryCountForm.getVulnerabilitiesCount() / (float) repositoryCountForm.getScanCount() * 100);
            }
            double s = Double.parseDouble(r);
            int star = s == 100.0 ? 5 : s > 0 && s < 20 ? 4 : s > 20 && s < 40 ? 3 : s > 40 && s < 60 ? 2 : 1;
            repositoryCountForm.setStar(star);
            return repositoryCountForm;
        }).collect(Collectors.toList());
    }

    @Override
    public RepositoryScannerForm repository(String storageId, String repositoryId, String artifactName, Integer page, Integer limit) {
        Pageable pageable = null;
        if (Objects.isNull(page)) {
            page = 1;
        }
        if (Objects.isNull(limit)) {
            limit = 5;
        }
        if (page == 1) {
            pageable = PageRequest.of(page, limit).first();
        } else {
            pageable = PageRequest.of(page, limit).previous();
        }
        String prefix = "%s-%s-";
        prefix = String.format(prefix, storageId, repositoryId);
        Repository repository = configurationManagementService.getConfiguration().getRepository(storageId, repositoryId);
        Page<Artifact> artifactPage = artifactRepository.scannerListByParams(pageable, artifactName, storageId, repositoryId);
        RepositoryScannerForm repositoryScannerForm = RepositoryScannerForm.builder().total(artifactPage.getTotalElements()).build();
        SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        repositoryScannerForm.setList(artifactPage.getContent().stream().map(artifact -> {
            String scanTime = DateUtil.format(Date.from(artifact.getScanDateTime().atZone(ZoneId.of("Asia/Shanghai")).toOffsetDateTime().toInstant()), df);
            RepositoryForm repositoryForm = RepositoryForm.builder().dependencyCount(artifact.getDependencyCount()).dependencyVulnerabilitiesCount(artifact.getDependencyVulnerabilitiesCount())
                    .uuid(artifact.getUuid()).scanTime(scanTime).suppressedVulnerabilitiesCount(artifact.getSuppressedVulnerabilitiesCount())
                    .vulnerabilitiesCount(artifact.getVulnerabilitiesCount()).storageId(artifact.getStorageId()).repositoryId(artifact.getRepositoryId()).artifactPath(artifact.getArtifactPath()).build();
            repositoryForm.setFilePaths(Optional.ofNullable(artifact.getFilePaths()).orElse(Collections.emptySet()).stream().map(item -> JSONObject.parseObject(item, ScannerReportForm.class)).collect(Collectors.toList()));
            if (DockerLayoutProvider.ALIAS.equals(repository.getLayout())) {
                //docker
                DockerCoordinates dockerArtifactCoordinates = (DockerCoordinates) artifact.getArtifactCoordinates();
                repositoryForm.setImageName(dockerArtifactCoordinates.getName());
                repositoryForm.setVersion(dockerArtifactCoordinates.getTAG());
            } else {
                if (CollectionUtils.isNotEmpty(repositoryForm.getFilePaths())) {
                    repositoryForm.setFilePath(repositoryForm.getFilePaths().get(0).getFilePath());
                }
            }
            return repositoryForm;
        }).collect(Collectors.toList()));
        return repositoryScannerForm;
    }

    @Override
    public void batchArtifactMetadata(List<ArtifactMetadataForm> artifactMetadataFormList) {
        // 批量的新增或更新 path Artifact 是一致的
        if (CollectionUtils.isNotEmpty(artifactMetadataFormList)) {
            ArtifactMetadataForm artifactMetaData = artifactMetadataFormList.get(0);
            Artifact artifact = null;
            String lockKey = String.format("%s-%s-%s-%s", "metadata", artifactMetaData.getStorageId(), artifactMetaData.getRepositoryId(), artifactMetaData.getArtifactPath());
            if (distributedLockComponent.lock(lockKey, GlobalConstants.WAIT_LOCK_TIME)) {
                try {
                    RepositoryPath repositoryPath = repositoryPathResolver.resolve(artifactMetaData.getStorageId(), artifactMetaData.getRepositoryId(), artifactMetaData.getArtifactPath());
                    if (Objects.isNull(repositoryPath) || !Files.exists(repositoryPath)) {
                        throw new BusinessException(String.format(GlobalConstants.ARTIFACT_NOT_FOUND_MESSAGE, artifactMetaData.getStorageId(), artifactMetaData.getRepositoryId(), artifactMetaData.getArtifactPath()));
                    }
                    validateAuth(repositoryPath);
                    boolean isDocker = DockerLayoutProvider.ALIAS.equalsIgnoreCase(repositoryPath.getRepository().getLayout());
                    boolean isDockerTag = isDocker && DockerCoordinates.isDockerTag(repositoryPath);
                    if (!isDockerTag && Files.isDirectory(repositoryPath)) {
                        recursiveBatchMetadata(repositoryPath, artifactMetaData, artifactMetadataFormList);
                        return;
                    }
                    artifact = resolvePath(artifactMetaData.getStorageId(), artifactMetaData.getRepositoryId(), artifactMetaData.getArtifactPath());
                    if (Objects.isNull(artifact)) {
                        throw new BusinessException(String.format(GlobalConstants.ARTIFACT_NOT_FOUND_MESSAGE, artifactMetaData.getStorageId(), artifactMetaData.getRepositoryId(), artifactMetaData.getArtifactPath()));
                    }
                    saveOrUpdateArtifactBatchMetadata(repositoryPath, artifact, artifactMetadataFormList);
                } catch (Exception e) {
                    log.error("Batch handle artifact metadata error [{}]", ExceptionUtils.getStackTrace(e));
                    if (e instanceof BusinessException) {
                        throw new RuntimeException(e.getMessage());
                    } else {
                        throw new RuntimeException("批量新增制品元数据错误，请稍后重试");
                    }
                } finally {
                    distributedLockComponent.unLock(lockKey);
                }
            } else {
                log.warn("Artifact [{}] was not get lock", lockKey);
            }
        }
    }

    /**
     * 获取制品元数据
     *
     * @param artifact artifact
     * @return 制品元数据
     * @throws IOException 异常
     */
    private JSONObject getMetadata(Artifact artifact) throws IOException {
        if (Objects.isNull(artifact)) {
            return null;
        }
        String metadata = artifact.getMetadata();
        JSONObject metadataJson = null;
        if (StringUtils.isNotBlank(metadata)) {
            metadataJson = JSONObject.parseObject(metadata);
        }
        log.debug("Current artifact [{}] metadata [{}]", artifact.getUuid(), Objects.nonNull(metadataJson) ? metadataJson.toJSONString() : "");
        return metadataJson;
    }

    /**
     * 获取docker Artifact 非镜像版本Artifact信息
     *
     * @param artifactName 制品名称
     * @param storageId    存储空间名称
     * @param repositoryId 仓库名称
     * @return docker Artifact 非镜像版本Artifact信息
     * @throws IOException 异常
     */
    private Artifact getDockerArtifact(String artifactName, String storageId, String repositoryId) throws IOException {
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactName);
        if (!Files.isDirectory(repositoryPath)) {
            return null;
        }
        DirectoryListing directoryListing = directoryListingService.fromRepositoryPath(repositoryPath);
        List<FileContent> fileContents = directoryListing.getFiles().stream().filter(file -> DockerCoordinates.include(file.getName())).collect(Collectors.toList());
        if (CollectionUtils.isEmpty(fileContents)) {
            return null;
        }
        FileContent fileContent = fileContents.get(0);
        String artifactPath = fileContent.getArtifactPath();
        return artifactRepository.findOneArtifact(storageId, repositoryId, artifactPath);
    }

    /***
     * 获取制品RepositoryPath
     * @param storageId 存储空间名称
     * @param repositoryId 仓库名称
     * @param artifactPath 制品路径
     * @return RepositoryPath
     */
    private Artifact resolvePath(String storageId, String repositoryId, String artifactPath) {
        Artifact artifact = null;
        try {
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
            artifact = Objects.nonNull(repositoryPath) ? repositoryPath.getArtifactEntry() : null;
            if (Objects.isNull(artifact)) {
                artifactPath = UriUtils.decode(artifactPath);
                repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
                artifact = Objects.nonNull(repositoryPath) ? repositoryPath.getArtifactEntry() : null;
            }
            if (Objects.isNull(artifact)) {
                //兼容已存在数据的docker布局仓库
                Repository repository = configurationManagementService.getConfiguration().getRepository(storageId, repositoryId);
                if (DockerLayoutProvider.ALIAS.equalsIgnoreCase(repository.getLayout())) {
                    //docker
                    artifact = getDockerArtifact(artifactPath, storageId, repositoryId);
                    return artifact;
                }
            }
        } catch (Exception ex) {
            log.warn(ExceptionUtils.getStackTrace(ex));
        }
        return artifact;
    }

    @Override
    public Artifact getArtifact(RepositoryPath repositoryPath) throws Exception {
        String repositoryId = repositoryPath.getRepository().getId();
        String storageId = repositoryPath.getStorageId();
        String artifactPath = repositoryPath.relativize().toString();
        return resolvePath(storageId, repositoryId, artifactPath);
    }



    @Override
    @Async("asyncThreadPoolTaskExecutor")
    public void buildGraphIndexForce(String username, String beginDate, String endDate, String storageId, String repositoryId, String storageIdAndRepositoryIds, String path, Boolean metadata, Integer batch) {
        log.info("BuildGraphIndexForce is starting...");
        try {
            LocalDateTime beginLocalDateTime = null, endLocalDateTime = null;
            if (StringUtils.isNotBlank(beginDate) && StringUtils.isNotBlank(endDate)) {
                beginLocalDateTime = DateUtil.parseLocalDateTime(beginDate, DatePattern.NORM_DATETIME_MINUTE_PATTERN);
                endLocalDateTime = DateUtil.parseLocalDateTime(endDate, DatePattern.NORM_DATETIME_MINUTE_PATTERN);
            }
            if (StringUtils.isNotBlank(storageIdAndRepositoryIds)) {
                for (String storageIdAndRepositoryId : storageIdAndRepositoryIds.split(GlobalConstants.COMMA)) {
                    storageId = ConfigurationUtils.getStorageId(storageIdAndRepositoryId, storageIdAndRepositoryId);
                    repositoryId = ConfigurationUtils.getRepositoryId(storageIdAndRepositoryId);
                    if (StringUtils.isNotBlank(storageId) && StringUtils.isNotBlank(repositoryId)) {
                        handleRepository(storageId, repositoryId, path, metadata, batch, beginLocalDateTime, endLocalDateTime, true);
                    }
                }
            } else if (StringUtils.isNotBlank(storageId) && StringUtils.isNotBlank(repositoryId)) {
                handleRepository(storageId, repositoryId, path, metadata, batch);
            } else if (StringUtils.isNotBlank(storageId)) {
                path = "";
                Map<String, ? extends Repository> repositoryMaps = configurationManagementService.getConfiguration().getStorage(storageId).getRepositories();
                if (!repositoryMaps.isEmpty()) {
                    for (String repository : repositoryMaps.keySet()) {
                        handleRepository(storageId, repository, path, metadata, batch, beginLocalDateTime, endLocalDateTime, true);
                    }
                }
            } else if (StringUtils.isBlank(storageId) && StringUtils.isBlank(repositoryId)) {
                path = "";
                Map<String, Storage> storageMap = configurationManagementService.getConfiguration().getStorages();
                if (!storageMap.isEmpty()) {
                    for (Map.Entry<String, Storage> storageEntry : storageMap.entrySet()) {
                        Map<String, ? extends Repository> repositoryMaps = configurationManagementService.getMutableConfigurationClone().getStorage(storageEntry.getKey()).getRepositories();
                        if (!repositoryMaps.isEmpty()) {
                            for (String repository : repositoryMaps.keySet()) {
                                handleRepository(storageEntry.getKey(), repository, path, metadata, batch, beginLocalDateTime, endLocalDateTime, true);
                            }
                        }
                    }
                }
            }
        } catch (Exception ex) {
            log.error("BuildGraphIndexForce is error [{}]", ExceptionUtils.getStackTrace(ex));
        }
        log.info("BuildGraphIndexForce is finished");
    }

    @Override
    public StatusInfo store(String username, String storageId, String repositoryId, String path, String uuid, MultipartFile file) {
        String parentPath = tempPath + File.separator + UUID.fastUUID().toString();
        File parentFile = new File(parentPath);
        StatusInfo statusInfo = StatusInfo.builder().total(0).success(0).fail(0).build();
        try (InputStream inputStream = file.getInputStream()) {
            String fileOriginalName = null;
            if (file instanceof FileStreamMultipartFile) {
                fileOriginalName = ((FileStreamMultipartFile) file).getOriginalFilename();
            } else if (file instanceof MultipartFile) {
                fileOriginalName = ((MultipartFile) file).getOriginalFilename();
            }

            String tempPath = parentPath + File.separator + fileOriginalName;
            File tempFile = new File(tempPath);
            FileUtil.writeFromStream(inputStream, tempFile);
            CompressUtils.unzip(tempFile.getAbsolutePath(), parentPath);
            RootRepositoryPath rootRepositoryPath = repositoryPathResolver.resolve(storageId, repositoryId);
            List<String> dirList = Lists.newArrayList();
            for (File f : Objects.requireNonNull(parentFile.listFiles())) {
                if (f.isDirectory()) {
                    dirList.add(f.getAbsolutePath());
                }
            }
            log.info("压缩包内扫描到的目录 [{}]", dirList);
            List<File> itemList, fileList = Lists.newArrayList();
            List<String> distributions = Lists.newArrayList();
            for (String dir : dirList) {
                itemList = getNFSFiles(dir, rootRepositoryPath.getRepository());
                if (CollectionUtils.isNotEmpty(itemList)) {
                    itemList = itemList.stream().filter(item -> artifactComponent.layoutSupports(rootRepositoryPath.getRepository().getLayout(), item.getAbsolutePath())).collect(Collectors.toList());
                    log.info("目录 [{}] 按照布局过滤后还有 [{}] 个文件", dir, itemList.size());
                    fileList.addAll(itemList);
                }
            }
            if (CollectionUtils.isNotEmpty(fileList)) {
                String filePath = "", separator = "/";
                int successTotal = 0;
                boolean flag = false;
                statusInfo.setTotal(fileList.size());
                for (File artifactFile : fileList) {
                    try {
                        filePath = artifactFile.getPath().substring(parentFile.getAbsolutePath().length());
                        if (filePath.startsWith(separator)) {
                            filePath = filePath.substring(1);
                        }
                        if (StringUtils.isNotBlank(path)) {
                            filePath = path + File.separator + filePath;
                        }
                        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, filePath);
                        if (!RepositoryFiles.isArtifact(repositoryPath)) {
                            log.warn("制品路径 [{}] 不是一个制品文件,跳过", repositoryPath.toString());
                            continue;
                        }
                        if(DebianLayoutProvider.ALIAS.equals(rootRepositoryPath.getRepository().getLayout())&&!filePath.endsWith(".deb")){
                            distributions.add(filePath);
                        }
                        try (FileInputStream fileInputStream = new FileInputStream(artifactFile)) {
                            flag = storeArtifact(repositoryPath, fileInputStream);
                            if (flag) {
                                successTotal = successTotal + 1;
                            }
                        }
                    } catch (Exception ex) {
                        log.error("路径 [{}] 错误 [{}] ", artifactFile.getAbsolutePath(), ExceptionUtils.getStackTrace(ex));
                        throw new RuntimeException(ex.getMessage());
                    }
                    statusInfo.setSuccess(successTotal);
                    statusInfo.setFail(statusInfo.getTotal() - statusInfo.getSuccess());
                }
                if(!distributions.isEmpty()){
                    for (String distribution : distributions) {
                        Matcher matcher = DebianConstant.PACKAGE_PATTERN.matcher(distribution);
                        if(matcher.matches()){
                            String codename = matcher.group("codename");
                            (new DebianReleaseMetadataIndexer(rootRepositoryPath.getRepository(), Collections.emptyList(), repositoryPathResolver)).indexRelease(codename);
                        }
                    }
                }
                if (ProductTypeEnum.Rpm.getFoLibraryName().equals(rootRepositoryPath.getRepository().getLayout())) {
                    RpmRepoIndexer rpmRepoIndexer = new RpmRepoIndexer(repositoryPathResolver, artifactManagementService, parentPath);
                    rpmRepoIndexer.indexWriter(rootRepositoryPath.getRepository());
                }
            }
            String status = JSONObject.toJSONString(statusInfo);
            log.info("操作账号 [{}] 本次状态 [{}]", username, status);
            handlerStatus(uuid, String.format("本次共扫描到%s个制品，保存成功%s个，失败%s个", statusInfo.getTotal(), statusInfo.getSuccess(), statusInfo.getFail()));
        } catch (Exception ex) {
            log.error("错误 [{}]", ExceptionUtils.getStackTrace(ex));
            handlerStatus(uuid, "发生错误，请稍候重试");
            throw new RuntimeException("发生错误，请稍候重试");
        } finally {
            try {
                FileUtil.del(parentFile);
                log.info("删除临时文件 [{}]", parentPath);
            } catch (IORuntimeException ex) {
                log.error("删除临时文件 [{}] 失败 [{}]", parentPath, ExceptionUtils.getStackTrace(ex));
            }
        }
        return statusInfo;
    }

    @Override
    public ArtifactStatistics artifactStatistics() {
        Long artifactsCount = artifactRepository.artifactsCount();
        Long artifactsBytes = artifactRepository.artifactsBytesStatistics(null);
        Long artifactsVulnerabilitiesCount = artifactRepository.artifactsVulnerabilitiesCount();
        Long criticalVulnerabilitiesCount = artifactRepository.criticalVulnerabilitiesCount();
        Long highVulnerabilitiesCount = artifactRepository.highVulnerabilitiesCount();
        Long mediumVulnerabilitiesCount = artifactRepository.mediumVulnerabilitiesCount();
        Long lowVulnerabilitiesCount = artifactRepository.lowVulnerabilitiesCount();
        Long suppressedVulnerabilitiesCount = artifactRepository.suppressedVulnerabilitiesCount();
        Long vulnerabilitiesCount = criticalVulnerabilitiesCount + highVulnerabilitiesCount + mediumVulnerabilitiesCount + lowVulnerabilitiesCount + suppressedVulnerabilitiesCount;
        return ArtifactStatistics.builder().artifactsCount(artifactsCount).artifactsVulnerabilitiesCount(artifactsVulnerabilitiesCount).artifactsNormalCount(artifactsCount - artifactsVulnerabilitiesCount)
                .criticalVulnerabilitiesCount(criticalVulnerabilitiesCount).highVulnerabilitiesCount(highVulnerabilitiesCount).mediumVulnerabilitiesCount(mediumVulnerabilitiesCount)
                .lowVulnerabilitiesCount(lowVulnerabilitiesCount).suppressedVulnerabilitiesCount(suppressedVulnerabilitiesCount).vulnerabilitiesCount(vulnerabilitiesCount).artifactsBytes(artifactsBytes).build();
    }

    @Override
    public TableResultResponse<ArtifactInfo> thirdPartyPage(ArtifactQuery artifactQuery) {
        Integer page = artifactQuery.getPage(), limit = artifactQuery.getLimit();
        String searchKeyword = artifactQuery.getSearchKeyword();
        if (Objects.isNull(page)) {
            page = 1;
        }
        if (Objects.isNull(limit)) {
            limit = 5;
        }
        Pageable pageable;
        if (page == 1) {
            pageable = PageRequest.of(page, limit).first();
        } else {
            pageable = PageRequest.of(page, limit).previous();
        }
        TableResultResponse<ArtifactInfo> tableResultResponse = new TableResultResponse<ArtifactInfo>(0, null);
        Page<Artifact> artifactPage = artifactRepository.findMatchingForThirdParty(pageable, searchKeyword);
        if (Objects.nonNull(artifactPage) && CollectionUtils.isNotEmpty(artifactPage.getContent())) {
            String baseUrl = configurationManagementService.getConfiguration().getBaseUrl();
            List<ArtifactInfo> artifactInfoList = Lists.newArrayList();
            ArtifactInfo artifactInfo = null;
            Repository repository = null;
            String download = "";
            RepositoryPath repositoryPath = null;
            for (Artifact artifact : artifactPage.getContent()) {
                repositoryPath = repositoryPathResolver.resolve(artifact.getStorageId(), artifact.getRepositoryId(), artifact.getArtifactPath());
                artifactInfo = ArtifactInfo.builder().build();
                artifactInfo.setRepo(String.format("%s/%s", artifact.getStorageId(), artifact.getRepositoryId()));
                artifactInfo.setPath(artifact.getArtifactPath());
                artifactInfo.setName(artifact.getArtifactName());
                repository = getRepository(artifact.getStorageId(), artifact.getRepositoryId());
                if (Objects.nonNull(repository) && Maven2LayoutProvider.ALIAS.equals(repository.getLayout())) {
                    if (artifact.getArtifactCoordinates() instanceof MavenCoordinates) {
                        MavenCoordinates mavenArtifactCoordinates = (MavenCoordinates) artifact.getArtifactCoordinates();
                        artifactInfo.setName(String.format("%s:%s", mavenArtifactCoordinates.getGroupId(), mavenArtifactCoordinates.getArtifactId()));
                    }
                } else if (Objects.nonNull(repository) && DockerLayoutProvider.ALIAS.equals(repository.getLayout())) {
                    if (artifact.getArtifactCoordinates() instanceof DockerCoordinates) {
                        DockerCoordinates dockerArtifactCoordinates = (DockerCoordinates) artifact.getArtifactCoordinates();
                        artifactInfo.setPath(dockerArtifactCoordinates.getIMAGE_NAME());
                        artifactInfo.setName(dockerArtifactCoordinates.getName());
                    }
                }
                artifactInfo.setDownload(getDownload(baseUrl, artifact.getStorageId(), artifact.getRepositoryId(), repository.getLayout(), repositoryPath, artifact));
                artifactInfo.setCreated(Date.from(artifact.getCreated().atZone(ZoneId.of("Asia/Shanghai")).toOffsetDateTime().toInstant()));
                artifactInfo.setUpdated(Date.from(artifact.getLastUpdated().atZone(ZoneId.of("Asia/Shanghai")).toOffsetDateTime().toInstant()));
                artifactInfo.setVersion(artifact.getArtifactCoordinates().getVersion());
                artifactInfo.setFormat(repository.getLayout());
                artifactInfo.setRepoType(repository.getType());
                artifactInfoList.add(artifactInfo);
            }
            tableResultResponse = new TableResultResponse<ArtifactInfo>(artifactPage.getTotalElements(), artifactInfoList);
        }
        return tableResultResponse;
    }

    @Override
    public void cleanupRepository(String storageId, String repositoryId, Boolean deleteFile, Integer limit) {
        if (Boolean.TRUE.equals(deleteFile)) {
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId);
            dropFiles(repositoryPath);
        }
        dropArtifact(storageId, repositoryId, limit);
    }

    @Override
    public List preview(RepositoryPath repositoryPath) {
        List result = null;
        try {
            Artifact updateArtifactEntry = repositoryPath.getArtifactEntry();
            if (Objects.isNull(updateArtifactEntry)) {
                return null;
            }
            Repository repository = repositoryPath.getRepository();
            LayoutProvider layoutProvider = layoutProviderRegistry.getProvider(repository.getLayout());
            Set<String> archiveFilenames = layoutProvider.listArchiveFilenames(repositoryPath);
            if (CollectionUtils.isNotEmpty(archiveFilenames)) {
                if (archiveFilenames.size() > 5) {
                    archiveFilenames = archiveFilenames.stream().limit(100).collect(Collectors.toSet());
                }
                ArtifactArchiveListing artifactArchiveListing = updateArtifactEntry.getArtifactArchiveListing();
                artifactArchiveListing.setFilenames(archiveFilenames);
                TreeUtil treeUtil = new TreeUtil();
                Set<String> fileNames = artifactArchiveListing.getFilenames();
                if (CollectionUtils.isNotEmpty(fileNames)) {
                    result = treeUtil.toTree(fileNames);
                }
                artifactService.saveOrUpdateArtifact(updateArtifactEntry);
            }
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
        return result;
    }

    @Override
    public void scan(RepositoryPath repositoryPath) {
        try {
            Artifact artifact = artifactRepository.findOneArtifactBase(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(repositoryPath));
            if (Objects.nonNull(artifact)) {
                scanService.syncScan(Lists.newArrayList(artifact));
            }
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
    }

    @Override
    public String dumpHead(String filePath) {
        try {
            if (StringUtils.isBlank(filePath)) {
                String filename = DateUtil.format(DateUtil.date(), DatePattern.PURE_DATETIME_PATTERN) + "_dump.hprof";
                filePath = tempPath + File.separator + "dumpHead" + File.separator + filename;
            }
            log.info("DumpHead file path [{}]", filePath);
            Path path = Path.of(filePath);
            Files.createDirectories(path.getParent());
            HotSpotDiagnosticMXBean bean = ManagementFactory.getPlatformMXBean(
                    HotSpotDiagnosticMXBean.class);
            bean.dumpHeap(filePath, true);
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
        return filePath;
    }

    @Override
    public void bomUpload(RepositoryPath repositoryPath, MultipartFile file) {
        RepositoryPath bomRepositoryPath = artifactComponent.getBomRepositoryPath(repositoryPath);
        try {
            log.info("Upload bom repositoryPath [{}] bomPath [{}]", repositoryPath.toString(), bomRepositoryPath.toString());
            Files.createDirectories(bomRepositoryPath.getParent());
            int batchSize = 1024;
            try (InputStream inputStream = file.getInputStream(); BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream, StandardCharsets.UTF_8))) {
                StringBuilder stringBuilder = new StringBuilder();
                char[] buffer = new char[batchSize];
                int charsRead;
                while ((charsRead = reader.read(buffer, 0, batchSize)) != -1) {
                    stringBuilder.append(buffer, 0, charsRead);
                }
                String bomContent = stringBuilder.toString();
                if (!JSONUtil.isJson(bomContent)) {
                    throw new IllegalArgumentException("BOM content must be in JSON format");
                }
                FoEyes foEyes = FoEyes.builder().uploadStatus(UploadStatusEnum.WAIT_UPLOAD.getType()).build();
                Bom bom = Bom.builder().bomId("").bomValue(JSONObject.parseObject(bomContent)).foEyes(foEyes).build();
                Files.write(bomRepositoryPath, JSONObject.toJSONString(bom).getBytes(StandardCharsets.UTF_8), StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.CREATE);
            }
            if (foEyesComponent.enable()) {
                //将SBOM传给foeyes
                foEyesComponent.bomUpload(repositoryPath, bomRepositoryPath);
            }
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException(ex.getMessage());
        }
    }

    @Override
    public boolean foEyesEnable() {
        return foEyesComponent.enable();
    }

    @Override
    public void dockerLayoutUpgrade(String storageId, String repositoryId, Boolean override) throws Exception {
        long startTime = System.currentTimeMillis();
        List<FutureTask<String>> futureTaskList = Lists.newArrayList();
        FutureTask<String> futureTask;
        Map<String, Storage> storageMap = configurationManagementService.getConfiguration().getStorages();
        if (MapUtils.isNotEmpty(storageMap)) {
            Repository repository;
            for (Map.Entry<String, Storage> entry : storageMap.entrySet()) {
                Map<String, ? extends Repository> repoMap = entry.getValue().getRepositories();
                if (MapUtils.isEmpty(repoMap)) {
                    continue;
                }
                if (StringUtils.isNotBlank(storageId) && !entry.getKey().equals(storageId)) {
                    continue;
                }
                for (Map.Entry<String, ? extends Repository> repoEntry : repoMap.entrySet()) {
                    repository = repoEntry.getValue();
                    if (!DockerLayoutProvider.ALIAS.equals(repository.getLayout())) {
                        continue;
                    }
                    if (StringUtils.isNotBlank(repositoryId) && !repoEntry.getKey().equals(repositoryId)) {
                        continue;
                    }
                    final RepositoryPath rootRepositoryPath = repositoryPathResolver.resolve(repository.getStorage().getId(), repository.getId());
                    if (!Files.exists(rootRepositoryPath)) {
                        continue;
                    }
                    final RepositoryPath blobsRootRepositoryPath = rootRepositoryPath.resolve(DockerLayoutProvider.BLOBS);
                    final RepositoryPath manifestRootRepositoryPath = rootRepositoryPath.resolve(DockerLayoutProvider.MANIFEST);
                    if (!Boolean.TRUE.equals(override) && Files.exists(blobsRootRepositoryPath) && Files.exists(manifestRootRepositoryPath)) {
                        continue;
                    }
                    futureTask = new FutureTask<String>(() -> {
                        handleDockerRepo(rootRepositoryPath, blobsRootRepositoryPath, manifestRootRepositoryPath);
                        return "";
                    });
                    futureTaskList.add(futureTask);
                    asyncThreadPoolTaskExecutor.submit(futureTask);
                }
            }
            for (FutureTask<String> task : futureTaskList) {
                task.get();
            }
            log.info("DockerLayoutUpgrade all is finished task time [{}] ms", System.currentTimeMillis() - startTime);
        }
    }

    @Override
    public void dockerIntegrity(String storageId, String repositoryId) throws Exception {
        long startTime = System.currentTimeMillis();
        List<FutureTask<String>> futureTaskList = Lists.newArrayList();
        FutureTask<String> futureTask;
        Map<String, Storage> storageMap = configurationManagementService.getConfiguration().getStorages();
        if (MapUtils.isNotEmpty(storageMap)) {
            Repository repository;
            String path = tempPath + File.separator + "dockerIntegrity" + File.separator + DateUtil.format(DateUtil.date(), DateTimeFormatter.ofPattern("yyyy-MM-dd-HH-mm")) + ".log";
            Path logPath = Path.of(path);
            Files.createDirectories(logPath.getParent());
            if (!Files.exists(logPath)) {
                Files.createFile(logPath);
            }
            ConcurrentFileWriterUtil concurrentFileWriterUtil = new ConcurrentFileWriterUtil(logPath);
            for (Map.Entry<String, Storage> entry : storageMap.entrySet()) {
                Map<String, ? extends Repository> repoMap = entry.getValue().getRepositories();
                if (MapUtils.isEmpty(repoMap)) {
                    continue;
                }
                if (StringUtils.isNotBlank(storageId) && !entry.getKey().equals(storageId)) {
                    continue;
                }
                for (Map.Entry<String, ? extends Repository> repoEntry : repoMap.entrySet()) {
                    repository = repoEntry.getValue();
                    if (!DockerLayoutProvider.ALIAS.equals(repository.getLayout()) || !RepositoryTypeEnum.HOSTED.getType().equals(repository.getType())) {
                        continue;
                    }
                    if (StringUtils.isNotBlank(repositoryId) && !repoEntry.getKey().equals(repositoryId)) {
                        continue;
                    }
                    final RepositoryPath rootRepositoryPath = repositoryPathResolver.resolve(repository.getStorage().getId(), repository.getId());
                    if (!Files.exists(rootRepositoryPath)) {
                        continue;
                    }
                    final RepositoryPath blobsRootRepositoryPath = rootRepositoryPath.resolve(DockerLayoutProvider.BLOBS);
                    final RepositoryPath manifestRootRepositoryPath = rootRepositoryPath.resolve(DockerLayoutProvider.MANIFEST);
                    futureTask = new FutureTask<String>(() -> {
                        handleDockerRepoIntegrity(concurrentFileWriterUtil, rootRepositoryPath, blobsRootRepositoryPath, manifestRootRepositoryPath);
                        return "";
                    });
                    futureTaskList.add(futureTask);
                    asyncThreadPoolTaskExecutor.submit(futureTask);
                }
            }
            for (FutureTask<String> task : futureTaskList) {
                task.get();
            }
            log.info("DockerIntegrity all is finished task time [{}] ms", System.currentTimeMillis() - startTime);
        }
    }

    private void handleDockerRepoIntegrity(ConcurrentFileWriterUtil concurrentFileWriterUtil, RepositoryPath rootRepositoryPath, RepositoryPath blobsRootRepositoryPath, RepositoryPath manifestRootRepositoryPath) {
        AtomicLong imagesAl = new AtomicLong(0), tagsAl = new AtomicLong(0),
                blobsAl = new AtomicLong(0), blobsArtifactAl = new AtomicLong(0),
                notExistBlobsAl = new AtomicLong(0), notExistBlobsArtifactAl = new AtomicLong(0),

                configBlobsAl = new AtomicLong(0), configBlobsArtifactAl = new AtomicLong(0),
                notExistConfigBlobsAl = new AtomicLong(0), notExistConfigBlobsArtifactAl = new AtomicLong(0),

                manifestsAl = new AtomicLong(0), manifestsArtifactAl = new AtomicLong(0),
                notExistManifestsAl = new AtomicLong(0), notExistManifestsArtifactAl = new AtomicLong(0),

                tagManifestsAl = new AtomicLong(0), tagManifestsArtifactAl = new AtomicLong(0),
                notExistTagManifestsAl = new AtomicLong(0), notExistTagManifestsArtifactAl = new AtomicLong(0);
        try {
            List<RepositoryPath> repositoryPathList = RepositoryPathUtil.getDockerImagePaths(rootRepositoryPath);
            for (RepositoryPath imageRepositoryPath : repositoryPathList) {
                try {
                    concurrentFileWriterUtil.write(String.format("Find image path [%s] [%s] [%s]", imageRepositoryPath.getStorageId(), imageRepositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(imageRepositoryPath)));
                    imagesAl.getAndIncrement();
                    //遍历镜像目录下的tag目录
                    List<String> excludeList = Lists.newArrayList(".temp");
                    excludeList.addAll(GlobalConstants.DOCKER_LAYER_DIR_NAME_LIST);
                    List<Path> tagList = dockerComponent.getTags(imageRepositoryPath, excludeList);
                    for (Path tagPath : tagList) {
                        RepositoryPath tagRepositoryPath = (RepositoryPath) tagPath;
                        concurrentFileWriterUtil.write(String.format("Find tag path [%s] [%s] [%s]", tagRepositoryPath.getStorageId(), tagRepositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(tagRepositoryPath)));
                        tagsAl.getAndIncrement();
                        try {
                            List<ImageManifest> imageManifestList = dockerComponent.getImageManifests(tagRepositoryPath);
                            if (CollectionUtils.isNotEmpty(imageManifestList)) {
                                RepositoryPath tagManifestRepositoryPath = dockerComponent.getManifestPath(tagRepositoryPath);
                                tagManifestsAl.getAndIncrement();
                                if (!Files.exists(tagManifestRepositoryPath)) {
                                    //manifest不存在
                                    concurrentFileWriterUtil.write(String.format("Find image [%s] [%s] [%s] tag manifest [%s] not exists", imageRepositoryPath.getStorageId(), imageRepositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(tagRepositoryPath), RepositoryFiles.relativizePath(tagManifestRepositoryPath)));
                                    notExistTagManifestsAl.getAndIncrement();
                                }
                                Artifact artifact = artifactRepository.findOneArtifact(tagManifestRepositoryPath.getStorageId(), tagManifestRepositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(tagManifestRepositoryPath));
                                if (Objects.isNull(artifact)) {
                                    concurrentFileWriterUtil.write(String.format("Find image [%s] [%s] [%s] tag manifest [%s] artifact not exists", imageRepositoryPath.getStorageId(), imageRepositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(tagRepositoryPath), RepositoryFiles.relativizePath(tagManifestRepositoryPath)));
                                    notExistTagManifestsArtifactAl.getAndIncrement();
                                } else {
                                    tagManifestsArtifactAl.getAndIncrement();
                                }
                                for (ImageManifest imageManifest : imageManifestList) {
                                    if (StringUtils.isNotBlank(imageManifest.getDigest())) {
                                        //manifest
                                        RepositoryPath manifestRepositoryPath = manifestRootRepositoryPath.resolve(imageManifest.getDigest());
                                        manifestsAl.getAndIncrement();
                                        if (!Files.exists(manifestRepositoryPath)) {
                                            //manifest不存在
                                            concurrentFileWriterUtil.write(String.format("Find image [%s] [%s] [%s] manifest [%s] not exists", imageRepositoryPath.getStorageId(), imageRepositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(tagRepositoryPath), RepositoryFiles.relativizePath(manifestRepositoryPath)));
                                            notExistManifestsAl.getAndIncrement();
                                        }
                                        Artifact manifestArtifact = artifactRepository.findOneArtifact(manifestRepositoryPath.getStorageId(), manifestRepositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(manifestRepositoryPath));
                                        if (Objects.isNull(manifestArtifact)) {
                                            concurrentFileWriterUtil.write(String.format("Find image [%s] [%s] [%s] manifest [%s] artifact not exists", imageRepositoryPath.getStorageId(), imageRepositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(tagRepositoryPath), RepositoryFiles.relativizePath(manifestRepositoryPath)));
                                            notExistManifestsArtifactAl.getAndIncrement();
                                        } else {
                                            manifestsArtifactAl.getAndIncrement();
                                        }
                                    }
                                    ContainerConfigurationManifest containerConfigurationManifest = imageManifest.getConfig();
                                    if (Objects.nonNull(containerConfigurationManifest) && StringUtils.isNotBlank(containerConfigurationManifest.getDigest())) {
                                        //config blob
                                        RepositoryPath configBlobRepositoryPath = blobsRootRepositoryPath.resolve(containerConfigurationManifest.getDigest());
                                        configBlobsAl.getAndIncrement();
                                        if (!Files.exists(configBlobRepositoryPath)) {
                                            //config blob不存在
                                            concurrentFileWriterUtil.write(String.format("Find image [%s] [%s] [%s] config blob [%s] not exists", imageRepositoryPath.getStorageId(), imageRepositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(tagRepositoryPath), RepositoryFiles.relativizePath(configBlobRepositoryPath)));
                                            notExistConfigBlobsAl.getAndIncrement();
                                        }
                                        Artifact blobArtifact = artifactRepository.findOneArtifact(configBlobRepositoryPath.getStorageId(), configBlobRepositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(configBlobRepositoryPath));
                                        if (Objects.isNull(blobArtifact)) {
                                            //config blob artifact不存在
                                            concurrentFileWriterUtil.write(String.format("Find image [%s] [%s] [%s] config blob [%s] artifact not exists", imageRepositoryPath.getStorageId(), imageRepositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(tagRepositoryPath), RepositoryFiles.relativizePath(configBlobRepositoryPath)));
                                            notExistConfigBlobsArtifactAl.getAndIncrement();
                                        } else {
                                            configBlobsArtifactAl.getAndIncrement();
                                        }
                                    }
                                    if (CollectionUtils.isNotEmpty(imageManifest.getLayers())) {
                                        for (LayerManifest layerManifest : imageManifest.getLayers()) {
                                            if (StringUtils.isNotBlank(layerManifest.getDigest())) {
                                                //layers blob
                                                RepositoryPath blobRepositoryPath = blobsRootRepositoryPath.resolve(layerManifest.getDigest());
                                                blobsAl.getAndIncrement();
                                                if (!Files.exists(blobRepositoryPath)) {
                                                    //layers blob不存在
                                                    concurrentFileWriterUtil.write(String.format("Find image [%s] [%s] [%s] layers blob [%s] not exists", imageRepositoryPath.getStorageId(), imageRepositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(tagRepositoryPath), RepositoryFiles.relativizePath(blobRepositoryPath)));
                                                    notExistBlobsAl.getAndIncrement();
                                                }
                                                Artifact blobArtifact = artifactRepository.findOneArtifact(blobRepositoryPath.getStorageId(), blobRepositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(blobRepositoryPath));
                                                if (Objects.isNull(blobArtifact)) {
                                                    //blob artifact不存在
                                                    concurrentFileWriterUtil.write(String.format("Find image [%s] [%s] [%s] layers blob [%s] artifact not exists", imageRepositoryPath.getStorageId(), imageRepositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(tagRepositoryPath), RepositoryFiles.relativizePath(blobRepositoryPath)));
                                                    notExistBlobsArtifactAl.getAndIncrement();
                                                } else {
                                                    blobsArtifactAl.getAndIncrement();
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        } catch (Exception ex) {
                            log.error("Handle image [{}] [{}] tag [{}] error [{}]", imageRepositoryPath.getStorageId(), imageRepositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(tagRepositoryPath), ExceptionUtils.getStackTrace(ex));
                        }
                    }
                } catch (Exception ex) {
                    log.error("Handle path [{}] [{}] [{}] error [{}]", imageRepositoryPath.getStorageId(), imageRepositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(imageRepositoryPath), ExceptionUtils.getStackTrace(ex));
                }
            }
            concurrentFileWriterUtil.write(String.format("DockerIntegrity [%s] [%s] is finished " +
                            "images [%s] tags [%s] " +
                            "blobs [%s] blobsArtifact [%s] " +
                            "notExistBlobs [%s] notExistBlobsArtifact [%s] " +
                            "configBlobs [%s] configBlobsArtifact [%s] " +
                            "notExistConfigBlobs [%s] notExistConfigBlobsArtifact [%s] " +
                            "manifests [%s] manifestsArtifact [%s] " +
                            "notExistManifests [%s] notExistManifestsArtifact [%s] " +
                            "tagManifests [%s] tagManifestsArtifact [%s] " +
                            "notExistTagManifests [%s] notExistTagManifestsArtifact [%s]", rootRepositoryPath.getStorageId(), rootRepositoryPath.getRepositoryId(),
                    imagesAl.get(), tagsAl.get(),
                    blobsAl.get(), blobsArtifactAl.get(),
                    notExistBlobsAl.get(), notExistBlobsArtifactAl.get(),
                    configBlobsAl.get(), configBlobsArtifactAl.get(),
                    notExistConfigBlobsAl.get(), notExistConfigBlobsArtifactAl.get(),
                    manifestsAl.get(), manifestsArtifactAl.get(),
                    notExistManifestsAl.get(), notExistManifestsArtifactAl.get(),
                    tagManifestsAl.get(), tagManifestsArtifactAl.get(),
                    notExistTagManifestsAl.get(), notExistTagManifestsArtifactAl.get()));
            if (notExistConfigBlobsAl.get() > 0 || notExistConfigBlobsArtifactAl.get() > 0 || notExistBlobsAl.get() > 0 || notExistBlobsArtifactAl.get() > 0 || notExistManifestsAl.get() > 0 || notExistManifestsArtifactAl.get() > 0 || notExistTagManifestsAl.get() > 0 || notExistTagManifestsArtifactAl.get() > 0) {
                concurrentFileWriterUtil.write(String.format("DockerIntegrity [%s] [%s] is finished " +
                                "images [%s] tags [%s] " +
                                "blobs [%s] blobsArtifact [%s] " +
                                "notExistBlobs [%s] notExistBlobsArtifact [%s] " +
                                "configBlobs [%s] configBlobsArtifact [%s] " +
                                "notExistConfigBlobs [%s] notExistConfigBlobsArtifact [%s] " +
                                "manifests [%s] manifestsArtifact [%s] " +
                                "notExistManifests [%s] notExistManifestsArtifact [%s] " +
                                "tagManifests [%s] tagManifestsArtifact [%s] " +
                                "notExistTagManifests [%s] notExistTagManifestsArtifact [%s] is not integrity", rootRepositoryPath.getStorageId(), rootRepositoryPath.getRepositoryId(),
                        imagesAl.get(), tagsAl.get(),
                        blobsAl.get(), blobsArtifactAl.get(),
                        notExistBlobsAl.get(), notExistBlobsArtifactAl.get(),
                        configBlobsAl.get(), configBlobsArtifactAl.get(),
                        notExistConfigBlobsAl.get(), notExistConfigBlobsArtifactAl.get(),
                        manifestsAl.get(), manifestsArtifactAl.get(),
                        notExistManifestsAl.get(), notExistManifestsArtifactAl.get(),
                        tagManifestsAl.get(), tagManifestsArtifactAl.get(),
                        notExistTagManifestsAl.get(), notExistTagManifestsArtifactAl.get()));
            }
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
    }

    @Override
    public void dockerLayoutUpgradeAll() throws Exception {
        dockerLayoutUpgrade(null, null, null);
    }

    @Override
    @org.springframework.transaction.annotation.Transactional
    public void deleteArtifactsResolve(String roleId, String resourceId) {
        List<String> roleIds = Lists.newArrayList(SystemRole.GENERAL.name());
        if (StringUtils.isNotBlank(roleId)) {
            roleIds.add(roleId);
        }
        if (StringUtils.isBlank(resourceId)) {
            resourceId = Privileges.ARTIFACTS_RESOLVE.getAuthority();
        }

        roleResourceRefMapper.delete(Wrappers.<RoleResourceRef>lambdaQuery()
                .in(RoleResourceRef::getRoleId, roleIds)
                .eq(RoleResourceRef::getResourceId, resourceId)
                .isNull(RoleResourceRef::getEntityId)
        );
    }

    @Override
    public void saveArtifactMetaByString(String storageId, String repositoryId, String path, String metaData) {
        try {
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
            if (Files.isDirectory(repositoryPath)) {
                artifactComponent.cacheArtifactMetadata(repositoryPath, metaData);
                return;
            }
            Artifact artifact = resolvePath(storageId, repositoryId, path);
            if (Objects.isNull(artifact)) {
                return;
            }
            artifact.setMetadata(metaData);
            artifactService.saveOrUpdateArtifact(artifact);
            repositoryPath.setArtifact(artifact);
            artifactEvent.dispatchArtifactMetaDataEvent(repositoryPath);
            cacheMetadata(repositoryPath);
        } catch (Exception e) {
            log.error("添加元数据失败 [{}]", ExceptionUtils.getStackTrace(e));
        }

    }

    @Override
    public long countByUUidPrefix(String uuidPrefix) {
        return artifactRepository.countByUUidPrefix(uuidPrefix);
    }

    @Override
    public String getMetadata(String storageId, String repositoryId, String path) {
        String metadata = "";
        try {
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
            if (!Files.exists(repositoryPath)) {
                return metadata;
            }
            if (Files.isDirectory(repositoryPath)) {
                metadata = artifactComponent.getCacheArtifactMetadata(repositoryPath);
            } else if (RepositoryFiles.isArtifact(repositoryPath)) {
                Artifact artifact = getArtifact(repositoryPath);
                if (Objects.isNull(artifact)) {
                    return metadata;
                }
                metadata = artifact.getMetadata();
            }
        } catch (Exception ex) {
            log.error("Get metadata storageId [{}] repositoryId [{}] path [{}] error [{}]", storageId, repositoryId, path, ExceptionUtils.getStackTrace(ex));
        }
        return metadata;
    }

    private void handleDockerRepo(RepositoryPath rootRepositoryPath, RepositoryPath blobsRootRepositoryPath, RepositoryPath manifestRootRepositoryPath) {
        AtomicLong imageAl = new AtomicLong(0), blobAl = new AtomicLong(0), manifestAl = new AtomicLong(0), copyBlobAl = new AtomicLong(0), copyManifestAl = new AtomicLong(0), copyBlobFailAl = new AtomicLong(0), copyManifestFailAl = new AtomicLong(0), deleteBlobAl = new AtomicLong(0), deleteManifestAl = new AtomicLong(0);
        try (Stream<Path> pathStream = Files.list(rootRepositoryPath)) {
            pathStream.forEach(path -> {
                try {
                    RepositoryPath imageRepositoryPath = (RepositoryPath) path;
                    String filename = imageRepositoryPath.getFileName().toString();
                    if (!filename.startsWith(GlobalConstants.POINT) && !RepositoryFiles.isTemp(imageRepositoryPath) && !Files.isHidden(imageRepositoryPath) && Files.isDirectory(imageRepositoryPath)) {
                        log.info("Find image path [{}] [{}] [{}]", imageRepositoryPath.getStorageId(), imageRepositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(imageRepositoryPath));
                        imageAl.getAndIncrement();
                        RepositoryPath blobsRepositoryPath = imageRepositoryPath.resolve(DockerLayoutProvider.BLOBS);
                        if (Files.exists(blobsRepositoryPath)) {
                            //遍历镜像目录下的blobs目录
                            try (Stream<Path> blobsPathStream = Files.list(blobsRepositoryPath)) {
                                blobsPathStream.forEach(blobsPath -> {
                                    RepositoryPath blobRepositoryPath = (RepositoryPath) blobsPath;
                                    try {
                                        String blobName = blobRepositoryPath.getFileName().toString();
                                        if (DockerCoordinates.include(blobName) && RepositoryFiles.isArtifact(blobRepositoryPath) && !RepositoryFiles.isArtifactChecksum(blobName) && !RepositoryFiles.isArtifactMetadata(blobRepositoryPath)) {
                                            RepositoryPath targetBlobRepositoryPath = blobsRootRepositoryPath.resolve(blobName);
                                            boolean exist = Files.exists(targetBlobRepositoryPath);
                                            log.info("Find image [{}] [{}] [{}] source blob [{}] target [{}] exists [{}]", imageRepositoryPath.getStorageId(), imageRepositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(imageRepositoryPath), RepositoryFiles.relativizePath(blobRepositoryPath), RepositoryFiles.relativizePath(targetBlobRepositoryPath), exist);
                                            blobAl.getAndIncrement();
                                            if (!exist) {
                                                //不存在，blob复制到仓库blobs目录下
                                                artifactManagementService.store(targetBlobRepositoryPath, blobRepositoryPath);
                                            }
                                            boolean fileExist = Files.exists(targetBlobRepositoryPath);
                                            Artifact artifact = artifactRepository.findOneArtifact(targetBlobRepositoryPath.getStorageId(), targetBlobRepositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(targetBlobRepositoryPath));
                                            boolean artifactExist = Objects.nonNull(artifact);
                                            if (fileExist && !artifactExist) {
                                                //文件存在、db中不存在
                                                artifactManagementService.validateAndStoreIndex(targetBlobRepositoryPath);
                                                artifact = artifactRepository.findOneArtifact(targetBlobRepositoryPath.getStorageId(), targetBlobRepositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(targetBlobRepositoryPath));
                                                artifactExist = Objects.nonNull(artifact);
                                            }
                                            if (fileExist && artifactExist) {
                                                //目标文件及db是否存在双重检查，都存在才可以删除源blob
                                                log.info("Find image [{}] [{}] [{}] source blob [{}] target [{}] copy finished", imageRepositoryPath.getStorageId(), imageRepositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(imageRepositoryPath), RepositoryFiles.relativizePath(blobRepositoryPath), RepositoryFiles.relativizePath(targetBlobRepositoryPath));
                                                copyBlobAl.getAndIncrement();
                                                //删除源blob
                                                RepositoryFiles.delete(blobRepositoryPath, true);
                                                log.info("Find image [{}] [{}] [{}] source blob [{}] target [{}] source deleted", imageRepositoryPath.getStorageId(), imageRepositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(imageRepositoryPath), RepositoryFiles.relativizePath(blobRepositoryPath), RepositoryFiles.relativizePath(targetBlobRepositoryPath));
                                                deleteBlobAl.getAndIncrement();
                                            } else {
                                                copyBlobFailAl.getAndIncrement();
                                                log.warn("Find image [{}] [{}] [{}] source blob [{}] target [{}] fileExist [{}] artifactExist [{}] copy failed", imageRepositoryPath.getStorageId(), imageRepositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(imageRepositoryPath), RepositoryFiles.relativizePath(blobRepositoryPath), RepositoryFiles.relativizePath(targetBlobRepositoryPath), fileExist, artifactExist);
                                            }
                                        }
                                    } catch (Exception ex) {
                                        log.error("Handle docker upgrade error image [{}] [{}] blob [{}] [{}]", imageRepositoryPath.getStorageId(), imageRepositoryPath.getRepositoryId(), blobRepositoryPath.toString(), ExceptionUtils.getStackTrace(ex));
                                    }
                                });
                            }
                            if (RepositoryFiles.isDirectoryEmpty(blobsRepositoryPath)) {
                                RepositoryFiles.delete(blobsRepositoryPath, true);
                            }
                        }
                        RepositoryPath manifestsRepositoryPath = imageRepositoryPath.resolve(DockerLayoutProvider.MANIFEST);
                        if (Files.exists(manifestsRepositoryPath)) {
                            //遍历镜像目录下的manifest目录
                            try (Stream<Path> manifestPathStream = Files.list(manifestsRepositoryPath)) {
                                manifestPathStream.forEach(manifestPath -> {
                                    RepositoryPath manifestRepositoryPath = (RepositoryPath) manifestPath;
                                    try {
                                        String manifestName = manifestRepositoryPath.getFileName().toString();
                                        if (DockerCoordinates.isRealManifestPath(manifestRepositoryPath)) {
                                            RepositoryPath targetManifestRepositoryPath = manifestRootRepositoryPath.resolve(manifestName);
                                            boolean exist = Files.exists(targetManifestRepositoryPath);
                                            log.info("Find image [{}] [{}] [{}] source blob [{}] target [{}] exists [{}]", imageRepositoryPath.getStorageId(), imageRepositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(imageRepositoryPath), RepositoryFiles.relativizePath(manifestRepositoryPath), RepositoryFiles.relativizePath(targetManifestRepositoryPath), exist);
                                            manifestAl.getAndIncrement();
                                            if (!exist) {
                                                //不存在，manifest复制到仓库manifest目录下
                                                dockerComponent.handleLayers(targetManifestRepositoryPath);
                                                artifactManagementService.store(targetManifestRepositoryPath, manifestRepositoryPath);
                                            }
                                            boolean fileExist = Files.exists(targetManifestRepositoryPath);
                                            Artifact artifact = artifactRepository.findOneArtifact(targetManifestRepositoryPath.getStorageId(), targetManifestRepositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(targetManifestRepositoryPath));
                                            boolean artifactExist = Objects.nonNull(artifact);
                                            if (fileExist && !artifactExist) {
                                                //文件存在、db中不存在
                                                artifactManagementService.validateAndStoreIndex(targetManifestRepositoryPath);
                                                artifact = artifactRepository.findOneArtifact(targetManifestRepositoryPath.getStorageId(), targetManifestRepositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(targetManifestRepositoryPath));
                                                artifactExist = Objects.nonNull(artifact);
                                            }
                                            if (fileExist && artifactExist) {
                                                //目标文件及db是否存在双重检查，都存在才可以删除源manifest
                                                log.info("Find image [{}] [{}] [{}] source manifest [{}] target [{}] copy finished", imageRepositoryPath.getStorageId(), imageRepositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(imageRepositoryPath), RepositoryFiles.relativizePath(manifestRepositoryPath), RepositoryFiles.relativizePath(targetManifestRepositoryPath));
                                                copyManifestAl.getAndIncrement();
                                                //删除源manifest
                                                RepositoryFiles.delete(manifestRepositoryPath, true);
                                                log.info("Find image [{}] [{}] [{}] source manifest [{}] target [{}] source deleted", imageRepositoryPath.getStorageId(), imageRepositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(imageRepositoryPath), RepositoryFiles.relativizePath(manifestRepositoryPath), RepositoryFiles.relativizePath(targetManifestRepositoryPath));
                                                deleteManifestAl.getAndIncrement();
                                            } else {
                                                copyManifestFailAl.getAndIncrement();
                                                log.warn("Find image [{}] [{}] [{}] source manifest [{}] target [{}] fileExist [{}] artifactExist [{}] copy failed", imageRepositoryPath.getStorageId(), imageRepositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(imageRepositoryPath), RepositoryFiles.relativizePath(manifestRepositoryPath), RepositoryFiles.relativizePath(targetManifestRepositoryPath), fileExist, artifactExist);
                                            }
                                        }
                                    } catch (Exception ex) {
                                        log.error("Handle docker upgrade error image [{}] [{}] manifest [{}] error [{}]", imageRepositoryPath.getStorageId(), imageRepositoryPath.getRepositoryId(), manifestRepositoryPath, ExceptionUtils.getStackTrace(ex));
                                    }
                                });
                            }
                            if (RepositoryFiles.isDirectoryEmpty(manifestsRepositoryPath)) {
                                RepositoryFiles.delete(manifestsRepositoryPath, true);
                            }
                        }
                    }
                } catch (Exception ex) {
                    log.error("Handle docker upgrade error path [{}] [{}]", path, ExceptionUtils.getStackTrace(ex));
                }
            });
            log.info("DockerLayoutUpgrade [{}] [{}] is finished images [{}] blobs [{}] manifest [{}] copyBlobs [{}] copyManifest [{}] copyBlobsFail [{}] copyManifestFail [{}] deleteBlobs [{}] deleteManifest [{}]", rootRepositoryPath.getStorageId(), rootRepositoryPath.getRepositoryId(), imageAl.get(), blobAl.get(), manifestAl.get(), copyBlobAl.get(), copyManifestAl.get(), copyBlobFailAl.get(), copyManifestFailAl.get(), deleteBlobAl.get(), deleteManifestAl.get());
            if (copyBlobFailAl.get() > 0 || copyManifestFailAl.get() > 0 || copyBlobAl.get() != deleteBlobAl.get() || copyManifestAl.get() != deleteManifestAl.get()) {
                log.warn("DockerLayoutUpgrade [{}] [{}] is finished images [{}] blobs [{}] manifest [{}] copyBlobs [{}] copyManifest [{}] copyBlobsFail [{}] copyManifestFail [{}] deleteBlobs [{}] deleteManifest [{}] upgrade incomplete with errors", rootRepositoryPath.getStorageId(), rootRepositoryPath.getRepositoryId(), imageAl.get(), blobAl.get(), manifestAl.get(), copyBlobAl.get(), copyManifestAl.get(), copyBlobFailAl.get(), copyManifestFailAl.get(), deleteBlobAl.get(), deleteManifestAl.get());
            }
        } catch (Exception ex) {
            log.error("Handle docker upgrade error [{}]", ExceptionUtils.getStackTrace(ex));
        }
    }

    @Override
    public void dockerLayoutDowngrade(String storageId, String repositoryId) throws Exception {
        List<FutureTask<String>> futureTaskList = Lists.newArrayList();
        FutureTask<String> futureTask;
        Map<String, Storage> storageMap = configurationManagementService.getConfiguration().getStorages();
        if (MapUtils.isNotEmpty(storageMap)) {
            Repository repository;
            for (Map.Entry<String, Storage> entry : storageMap.entrySet()) {
                Map<String, ? extends Repository> repoMap = entry.getValue().getRepositories();
                if (MapUtils.isEmpty(repoMap)) {
                    continue;
                }
                if (StringUtils.isNotBlank(storageId) && !entry.getKey().equals(storageId)) {
                    continue;
                }
                for (Map.Entry<String, ? extends Repository> repoEntry : repoMap.entrySet()) {
                    repository = repoEntry.getValue();
                    if (!DockerLayoutProvider.ALIAS.equals(repository.getLayout())) {
                        continue;
                    }
                    if (StringUtils.isNotBlank(repositoryId) && !repoEntry.getKey().equals(repositoryId)) {
                        continue;
                    }
                    final RepositoryPath rootRepositoryPath = repositoryPathResolver.resolve(repository.getStorage().getId(), repository.getId());
                    if (!Files.exists(rootRepositoryPath)) {
                        continue;
                    }
                    final RepositoryPath blobsRootRepositoryPath = rootRepositoryPath.resolve(DockerLayoutProvider.BLOBS);
                    final RepositoryPath manifestRootRepositoryPath = rootRepositoryPath.resolve(DockerLayoutProvider.MANIFEST);
                    futureTask = new FutureTask<String>(() -> {
                        handleDockerRepoDowngrade(rootRepositoryPath, blobsRootRepositoryPath, manifestRootRepositoryPath);
                        return "";
                    });
                    futureTaskList.add(futureTask);
                    asyncThreadPoolTaskExecutor.submit(futureTask);
                }
            }
            for (FutureTask<String> task : futureTaskList) {
                task.get();
            }
            log.info("DockerLayoutDowngrade all is finished");
        }
    }


    private void handleDockerRepoDowngrade(RepositoryPath rootRepositoryPath, RepositoryPath blobsRootRepositoryPath, RepositoryPath manifestRootRepositoryPath) {
        AtomicLong imageAl = new AtomicLong(0), blobAl = new AtomicLong(0), manifestAl = new AtomicLong(0), copyBlobAl = new AtomicLong(0), copyManifestAl = new AtomicLong(0), deleteBlobAl = new AtomicLong(0), deleteManifestAl = new AtomicLong(0), rootBlobAl = new AtomicLong(0), rootManifestAl = new AtomicLong(0);
        try (Stream<Path> pathStream = Files.list(rootRepositoryPath)) {
            pathStream.filter(item -> GlobalConstants.DOCKER_LAYER_DIR_NAME_LIST.stream().noneMatch(dir -> dir.equals(item.getFileName().toString()))).forEach(path -> {
                try {
                    RepositoryPath imageRepositoryPath = (RepositoryPath) path;
                    String filename = imageRepositoryPath.getFileName().toString();
                    if (!filename.startsWith(GlobalConstants.POINT) && !RepositoryFiles.isTemp(imageRepositoryPath) && !Files.isHidden(imageRepositoryPath) && Files.isDirectory(imageRepositoryPath)) {
                        log.info("Find image path [{}] [{}] [{}]", imageRepositoryPath.getStorageId(), imageRepositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(imageRepositoryPath));
                        imageAl.getAndIncrement();
                        if (Files.exists(imageRepositoryPath)) {
                            //遍历镜像目录下的tag目录
                            try (Stream<Path> tagPathStream = Files.list(imageRepositoryPath)) {
                                tagPathStream.forEach(tagPath -> {
                                    RepositoryPath tagRepositoryPath = (RepositoryPath) tagPath;
                                    try {
                                        List<ImageManifest> imageManifestList = dockerComponent.getImageManifests(tagRepositoryPath);
                                        if (CollectionUtils.isNotEmpty(imageManifestList)) {
                                            for (ImageManifest imageManifest : imageManifestList) {
                                                if (StringUtils.isNotBlank(imageManifest.getDigest())) {
                                                    //manifest
                                                    RepositoryPath sourceManifestRepositoryPath = manifestRootRepositoryPath.resolve(imageManifest.getDigest());
                                                    if (Files.exists(sourceManifestRepositoryPath)) {
                                                        RepositoryPath targetManifestRepositoryPath = imageRepositoryPath.resolve(DockerLayoutProvider.MANIFEST + File.separator + imageManifest.getDigest());
                                                        boolean exists = Files.exists(targetManifestRepositoryPath);
                                                        log.info("Find image [{}] [{}] [{}] source manifest [{}] target [{}] exists [{}]", imageRepositoryPath.getStorageId(), imageRepositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(tagRepositoryPath), RepositoryFiles.relativizePath(sourceManifestRepositoryPath), RepositoryFiles.relativizePath(targetManifestRepositoryPath), exists);
                                                        manifestAl.getAndIncrement();
                                                        if (!exists) {
                                                            //不存在，manifest复制到镜像manifest目录下
                                                            artifactManagementService.store(targetManifestRepositoryPath, sourceManifestRepositoryPath);
                                                            log.info("Find image [{}] [{}] [{}] source manifest [{}] target [{}] copy finished", imageRepositoryPath.getStorageId(), imageRepositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(tagRepositoryPath), RepositoryFiles.relativizePath(sourceManifestRepositoryPath), RepositoryFiles.relativizePath(targetManifestRepositoryPath));
                                                            copyManifestAl.getAndIncrement();
                                                        }
                                                    }
                                                }
                                                ContainerConfigurationManifest containerConfigurationManifest = imageManifest.getConfig();
                                                if (Objects.nonNull(containerConfigurationManifest) && StringUtils.isNotBlank(containerConfigurationManifest.getDigest())) {
                                                    //config blob
                                                    RepositoryPath sourceConfigRepositoryPath = blobsRootRepositoryPath.resolve(containerConfigurationManifest.getDigest());
                                                    if (Files.exists(sourceConfigRepositoryPath)) {
                                                        RepositoryPath targetConfigRepositoryPath = imageRepositoryPath.resolve(DockerLayoutProvider.BLOBS + File.separator + containerConfigurationManifest.getDigest());
                                                        boolean exists = Files.exists(targetConfigRepositoryPath);
                                                        log.info("Find image [{}] [{}] [{}] source config blob [{}] target [{}] exists [{}]", imageRepositoryPath.getStorageId(), imageRepositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(tagRepositoryPath), RepositoryFiles.relativizePath(sourceConfigRepositoryPath), RepositoryFiles.relativizePath(targetConfigRepositoryPath), exists);
                                                        blobAl.getAndIncrement();
                                                        if (!exists) {
                                                            //不存在，config blob复制到镜像blobs目录下
                                                            artifactManagementService.store(targetConfigRepositoryPath, sourceConfigRepositoryPath);
                                                            log.info("Find image [{}] [{}] [{}] source config blob [{}] target [{}] copy finished", imageRepositoryPath.getStorageId(), imageRepositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(tagRepositoryPath), RepositoryFiles.relativizePath(sourceConfigRepositoryPath), RepositoryFiles.relativizePath(targetConfigRepositoryPath));
                                                            copyBlobAl.getAndIncrement();
                                                        }
                                                    }
                                                }
                                                if (CollectionUtils.isNotEmpty(imageManifest.getLayers())) {
                                                    for (LayerManifest layerManifest : imageManifest.getLayers()) {
                                                        if (StringUtils.isNotBlank(layerManifest.getDigest())) {
                                                            //layers blob
                                                            RepositoryPath sourceBlobRepositoryPath = blobsRootRepositoryPath.resolve(layerManifest.getDigest());
                                                            if (Files.exists(sourceBlobRepositoryPath)) {
                                                                RepositoryPath targetBlobRepositoryPath = imageRepositoryPath.resolve(DockerLayoutProvider.BLOBS + File.separator + layerManifest.getDigest());
                                                                boolean exists = Files.exists(targetBlobRepositoryPath);
                                                                log.info("Find image [{}] [{}] [{}] source blob [{}] target [{}] exists [{}]", imageRepositoryPath.getStorageId(), imageRepositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(tagRepositoryPath), RepositoryFiles.relativizePath(sourceBlobRepositoryPath), RepositoryFiles.relativizePath(targetBlobRepositoryPath), exists);
                                                                blobAl.getAndIncrement();
                                                                if (!exists) {
                                                                    //不存在，blob复制到镜像blobs目录下
                                                                    artifactManagementService.store(targetBlobRepositoryPath, sourceBlobRepositoryPath);
                                                                    log.info("Find image [{}] [{}] [{}] source blob [{}] target [{}] copy finished", imageRepositoryPath.getStorageId(), imageRepositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(tagRepositoryPath), RepositoryFiles.relativizePath(sourceBlobRepositoryPath), RepositoryFiles.relativizePath(targetBlobRepositoryPath));
                                                                    copyBlobAl.getAndIncrement();
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    } catch (Exception ex) {
                                        log.error("Handle image [{}] [{}] tag [{}] error [{}]", imageRepositoryPath.getStorageId(), imageRepositoryPath.getRepositoryId(), tagRepositoryPath, ExceptionUtils.getStackTrace(ex));
                                    }
                                });
                            }
                        }
                    }
                } catch (Exception ex) {
                    log.error("Handle path [{}] error [{}]", path, ExceptionUtils.getStackTrace(ex));
                }
            });
            //删除仓库根目录下blobs目录
            try (Stream<Path> blobStream = Files.list(blobsRootRepositoryPath)) {
                blobStream.forEach(blobPath -> {
                    if (DockerCoordinates.include(blobPath.getFileName().toString())) {
                        rootBlobAl.getAndIncrement();
                        try {
                            RepositoryFiles.delete((RepositoryPath) blobPath, true);
                            deleteBlobAl.getAndIncrement();
                        } catch (Exception ex) {
                            log.error(ExceptionUtils.getStackTrace(ex));
                        }
                    }
                });
            }
            if (RepositoryFiles.isDirectoryEmpty(blobsRootRepositoryPath)) {
                RepositoryFiles.delete(blobsRootRepositoryPath, true);
            }
            //删除仓库根目录下manifest目录
            try (Stream<Path> manifestStream = Files.list(manifestRootRepositoryPath)) {
                manifestStream.forEach(manifestPath -> {
                    if (DockerCoordinates.include(manifestPath.getFileName().toString())) {
                        rootManifestAl.getAndIncrement();
                        try {
                            RepositoryFiles.delete((RepositoryPath) manifestPath, true);
                            deleteManifestAl.getAndIncrement();
                        } catch (Exception ex) {
                            log.error(ExceptionUtils.getStackTrace(ex));
                        }
                    }
                });
            }
            if (RepositoryFiles.isDirectoryEmpty(manifestRootRepositoryPath)) {
                RepositoryFiles.delete(manifestRootRepositoryPath, true);
            }
            log.info("DockerLayoutDowngrade [{}] is finished images [{}] blobs [{}] manifest [{}] copyBlobs [{}] copyManifest [{}] rootBlobs [{}] rootManifest [{}] deleteBlobs [{}] deleteManifest [{}]", rootRepositoryPath.toString(), imageAl.get(), blobAl.get(), manifestAl.get(), copyBlobAl.get(), copyManifestAl.get(), rootBlobAl.get(), rootManifestAl.get(), deleteBlobAl.get(), deleteManifestAl.get());
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
    }

    /**
     * 清空仓库
     *
     * @param repositoryPath 仓库路径
     */
    private void dropFiles(RepositoryPath repositoryPath) {
        try {
            RootRepositoryPath root = repositoryPath.getFileSystem().getRootDirectory();
            Files.walkFileTree(repositoryPath, new SimpleFileVisitor<Path>() {
                @Override
                public FileVisitResult visitFile(Path file,
                                                 BasicFileAttributes attrs)
                        throws IOException {
                    Files.delete(file);
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult postVisitDirectory(Path dir,
                                                          IOException exc)
                        throws IOException {
                    if (root.equals(dir)) {
                        return FileVisitResult.CONTINUE;
                    }
                    try {
                        Files.delete(dir);
                    } catch (DirectoryNotEmptyException e) {
                        try (Stream<Path> pathStream = Files.list(dir)) {
                            String message = pathStream
                                    .map(p -> p.getFileName().toString())
                                    .reduce((p1,
                                             p2) -> String.format("%s%n%s", p1, p2))
                                    .get();
                            throw new IOException(message, e);
                        }
                    }
                    return FileVisitResult.CONTINUE;
                }
            });
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
    }

    /**
     * 清空db
     *
     * @param storageId    存储空间
     * @param repositoryId 仓库
     * @param limit        批处理数量
     */
    private void dropArtifact(String storageId, String repositoryId, Integer limit) {
        Long count = artifactRepository.artifactsCount(storageId, repositoryId);
        log.info("DropArtifact storageId [{}] repositoryId [{}] count [{}] limit [{}]", storageId, repositoryId, count, limit);
        long deleteCount = Long.parseLong("0");
        while (count > 0) {
            artifactRepository.dropArtifacts(storageId, repositoryId, limit);
            count = artifactRepository.artifactsCount(storageId, repositoryId);
            deleteCount = deleteCount + 1;
            log.info("DropArtifact storageId [{}] repositoryId [{}] count [{}] limit [{}] deleteCount [{}]", storageId, repositoryId, count, limit, deleteCount);
        }
    }

    private String getDownload(String baseUrl, String storageId, String repositoryId, String layout, RepositoryPath repositoryPath, Artifact artifact) {
        try {
            String storage = "storages";
            if (DockerLayoutProvider.ALIAS.equals(layout)) {
                if (artifact.getArtifactCoordinates() instanceof DockerCoordinates) {
                    DockerCoordinates dockerArtifactCoordinates = (DockerCoordinates) artifact.getArtifactCoordinates();
                    baseUrl = StringUtils.removeEnd(baseUrl, "/");
                    return String.format("%s/%s/%s/%s/%s/%s/%s", baseUrl, "v2", storageId, repositoryId, dockerArtifactCoordinates.getName(), "manifests", dockerArtifactCoordinates.getTAG());
                }
                return "";
            }
            URI artifactResource = RepositoryFiles.resolveResource(repositoryPath);
            return UriComponentsBuilder.fromUri(URI.create(baseUrl))
                    .pathSegment(storage, storageId, repositoryId, "/")
                    .build()
                    .toUri()
                    .resolve(artifactResource)
                    .toURL().toString();
        } catch (Exception ex) {
            log.warn("获取repositoryPath [{}] URI错误：[{}]", repositoryPath.toString(), ExceptionUtils.getStackTrace(ex));
        }
        return "";
    }

    /**
     * 处理上传状态信息
     *
     * @param uuid    uuid
     * @param comment 异常信息
     */
    private void handlerStatus(String uuid, String comment) {
        if (StringUtils.isNotBlank(uuid)) {
            dictService.saveOrUpdateDict(Dict.builder().dictKey(uuid).comment(comment).build(), null);
        }
    }

    /**
     * 存储制品
     *
     * @param repositoryPath 制品路径
     */
    private boolean storeArtifact(RepositoryPath repositoryPath, InputStream inputStream) {
        try {
            artifactManagementService.validateAndStore(repositoryPath, inputStream);
            try {
                artifactMetadataService.rebuildMetadata(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(repositoryPath));
            } catch (Exception ex) {
                log.error("StoreArtifact rebuildMetadata repositoryPath：{}，error：{}", repositoryPath.toString(), ExceptionUtils.getStackTrace(ex));
                throw ex;
            }
        } catch (Exception ex) {
            log.error("StoreArtifact repositoryPath：{} error：{}", repositoryPath.toString(), ExceptionUtils.getStackTrace(ex));
            throw  new RuntimeException(ex);
        }
        return true;
    }

    /**
     * 单仓库
     *
     * @param storageId    存储空间
     * @param repositoryId 仓库id
     * @param path         path
     * @param metadata     是否同步元数据 true 是 false 否
     * @param batch        每批数量
     * @param beginDate    开始日期
     * @param endDate      结束日期
     * @param force        是否强制 true 强制 其他不强制
     */
    private void handleRepository(String storageId, String repositoryId, String path, Boolean metadata, Integer batch, LocalDateTime beginDate, LocalDateTime endDate, Boolean force) {
        try {
            log.info("StorageId [{}]，repositoryId [{}] starting...", storageId, repositoryId);
            RootRepositoryPath rootRepositoryPath = repositoryPathResolver.resolve(storageId, repositoryId);
            RepositoryPath repositoryPath = rootRepositoryPath;
            if (StringUtils.isNotBlank(path)) {
                repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
            }
            if (Objects.isNull(batch)) {
                batch = 500;
            }
            Repository repository = repositoryPath.getRepository();
            handleArtifacts(repositoryPath, repository, metadata, batch, beginDate, endDate, force);
            if (ProductTypeEnum.Rpm.getFoLibraryName().equals(repository.getLayout())) {
                RpmRepoIndexer rpmRepoIndexer = new RpmRepoIndexer(repositoryPathResolver, artifactManagementService, tempPath);
                rpmRepoIndexer.indexWriter(repository);
            }
            log.info("StorageId [{}] repositoryId [{}] is finished", storageId, repositoryId);
        } catch (Exception ex) {
            log.error("StorageId [{}] repositoryId [{}] error [{}]", storageId, repositoryId, ExceptionUtils.getStackTrace(ex));
        }
    }

    /**
     * 单仓库
     *
     * @param storageId    存储空间
     * @param repositoryId 仓库id
     * @param path         path
     * @param metadata     是否同步元数据 true 是 false 否
     * @param batch        每批数量
     */
    private void handleRepository(String storageId, String repositoryId, String path, Boolean metadata, Integer batch) {
        handleRepository(storageId, repositoryId, path, metadata, batch, null, null, null);
    }

    public Set<String> roleNames(Authentication authentication) {
        SpringSecurityUser userDetails = (SpringSecurityUser) authentication.getPrincipal();
        return Optional.ofNullable(userDetails.getRoles()).orElse(Collections.emptySet()).stream().map(Role::getName).collect(Collectors.toSet());
    }

    /**
     * 获取有权限访问的存储空间id列表
     *
     * @return 有权限访问的存储空间id列表
     */
    public List<String> havePermissionStorageIdList(Authentication authentication) {
        List<String> storageIdList = Lists.newArrayList();
        Set<String> roleNames = roleNames(authentication);
        final UserDetails loggedUser = (UserDetails) authentication.getPrincipal();
        String username = loggedUser.getUsername();
        if (roleNames.contains(SystemRole.ADMIN.name())) {
            storageIdList = new ArrayList<>(configurationManagementService.getConfiguration().getStorages().keySet());
            return storageIdList;
        }
        for (Map.Entry<String, Storage> entry : configurationManagementService.getConfiguration().getStorages().entrySet()) {
            //查询数据库中存储空间绑定的用户
            String storageId = entry.getKey();
            Map<String, Set<String>> storageUser = storageManagementService.getStorageUser(Collections.singleton(storageId));
            Set<String> userSet = storageUser.get(storageId);
            if (CollectionUtils.isNotEmpty(userSet)) {
                if (userSet.contains(username)) {
                    storageIdList.add(entry.getKey());
                } else if (Objects.nonNull(entry.getValue().getRepositories()) && entry.getValue().getRepositories().values().stream().anyMatch(item -> RepositoryScopeEnum.OPEN.getType().equals(item.getScope()))) {
                    storageIdList.add(entry.getKey());
                }
            }
        }
        return storageIdList;
    }

    /**
     * 获取仓库名称集合
     *
     * @param onScan     扫描状态  1 扫描开启 0 扫描关闭
     * @param storageIds 存储空间集合
     * @return 仓库名称集合
     */
    private List<String> getStorageIdsRepositoryIdsByOnScanAndStorageIds(Integer onScan, List<String> storageIds) {
        if (CollectionUtils.isEmpty(storageIds)) {
            return Collections.emptyList();
        }
        List<String> storageIdAndRepositoryIdList = getStorageIdAndRepositoryIdList(storageIds);
        if (CollectionUtils.isEmpty(storageIdAndRepositoryIdList)) {
            return Collections.emptyList();
        }
        List<ScanRules> scanRulesList = scanRulesMapper.selectList(Wrappers.<ScanRules>lambdaQuery()
                .eq(ScanRules::getOnScan, onScan)
                .in(ScanRules::getId, storageIdAndRepositoryIdList)
        );
        return Optional.ofNullable(scanRulesList).orElse(Collections.emptyList()).stream().map(item -> String.format("%s-%s", item.getStorage(), item.getRepository())).collect(Collectors.toList());
    }

    private Long getStartLong(String date) {
        LocalDateTime startLocalDateTime = DateUtil.parseLocalDateTime(date + " 00:00:00", DatePattern.NORM_DATETIME_PATTERN);
        return EntityTraversalUtils.toLong(startLocalDateTime);
    }

    private Long getEndLong(String date) {
        LocalDateTime endLocalDateTime = DateUtil.parseLocalDateTime(date + " 23:59:59", DatePattern.NORM_DATETIME_PATTERN);
        return EntityTraversalUtils.toLong(endLocalDateTime);
    }

    private List<File> getNFSFiles(String path, Repository repository) {
        boolean dockerLayout = DockerLayoutProvider.ALIAS.equalsIgnoreCase(repository.getLayout());
        int fileNum = 0, folderNum = 0;
        File rootFile = new File(path);
        if (rootFile.isHidden()) {
            log.info("root file：{} is a hidden file", rootFile.getName());
            return Collections.emptyList();
        }
        LinkedList<File> list = new LinkedList<>();
        List<File> resultList = new ArrayList<>();
        if (rootFile.exists()) {
            if (null == rootFile.listFiles() && rootFile.isFile()) {
                if (dockerLayout && !rootFile.getName().contains("sha256")) {
                    log.info("file：{} not is a docker layout file", rootFile.getName());
                    return Collections.emptyList();
                }
                if (RepositoryFiles.isArtifactChecksum(rootFile.getName())) {
                    log.info("file {} is a checksum file skip", rootFile.getName());
                    return Collections.emptyList();
                }
                log.info("file:{}", rootFile.getAbsolutePath());
                resultList.add(rootFile);
                fileNum++;
            } else if (Objects.nonNull(rootFile.listFiles())) {
                for (File f : rootFile.listFiles()) {
                    if (f.isDirectory()) {
                        if (f.isHidden()) {
                            log.info("directory：{} is a hidden directory skip", f.getName());
                            continue;
                        }
                        if (f.getName().endsWith(".artifactory-metadata")) {
                            log.info("directory：{} is a artifactory metadata directory skip", f.getName());
                            continue;
                        }
                        list.add(f);
                        folderNum++;
                    } else {
                        if (f.isHidden()) {
                            log.info("file：{} is a hidden file", f.getName());
                            continue;
                        }
                        if (dockerLayout && !f.getName().contains("sha256")) {
                            log.info("file：{} not is a docker layout file", f.getName());
                            continue;
                        }
                        if (RepositoryFiles.isArtifactChecksum(f.getName())) {
                            log.info("file {} is a checksum file skip", f.getName());
                            continue;
                        }
                        log.info("file:{}", f.getAbsolutePath());
                        resultList.add(f);
                        fileNum++;
                    }
                }
            }
            while (!list.isEmpty()) {
                File[] files = list.removeFirst().listFiles();
                if (null == files) {
                    continue;
                }
                for (File f : files) {
                    if (f.isDirectory()) {
                        if (f.isHidden()) {
                            log.info("directory：{} is a hidden directory skip", f.getName());
                            continue;
                        }
                        if (f.getName().endsWith(".artifactory-metadata")) {
                            log.info("directory：{} is a artifactory metadata directory skip", f.getName());
                            continue;
                        }
                        log.info("directory:{}", f.getAbsolutePath());
                        list.add(f);
                        folderNum++;
                    } else {
                        if (f.isHidden()) {
                            log.info("file：{} is a hidden file", f.getName());
                            continue;
                        }
                        if (dockerLayout && !f.getName().contains("sha256")) {
                            log.info("file：{} not is a docker layout file", f.getName());
                            continue;
                        }
                        if (RepositoryFiles.isArtifactChecksum(f.getName())) {
                            log.info("file {} is a checksum file skip", f.getName());
                            continue;
                        }
                        log.info("file:{}", f.getAbsolutePath());
                        resultList.add(f);
                        fileNum++;
                    }
                }
            }
        } else {
            log.info("file {} not exists!", path);
        }
        log.info("Path：{} directory size:{} ,file size:{}", path, folderNum, fileNum);
        return resultList;
    }

    /**
     * 处理存储制品
     *
     * @param rootRepositoryPath rootRepositoryPath
     * @param repository         仓库信息
     * @param metadata           是否同步元数据 true 是 false 否
     * @param batch              每批数量
     * @param beginDate          开始日期
     * @param endDate            结束日期
     * @param force              是否强制 true 强制 其他不强制
     * @return NFS目录下的所有文件
     */
    private List<RepositoryPath> handleArtifacts(RepositoryPath rootRepositoryPath, Repository repository, Boolean metadata, Integer batch, LocalDateTime beginDate, LocalDateTime endDate, Boolean force) throws Exception {
        List<RepositoryPath> resultList = RepositoryPathUtil.getPaths(repository.getLayout(), rootRepositoryPath, Lists.newArrayList(DockerLayoutProvider.BLOBS, DockerLayoutProvider.MANIFEST), beginDate, endDate);
        List<List<RepositoryPath>> fileLists = Lists.partition(resultList, batch);
        List<FutureTask<String>> futureTaskList = Lists.newArrayList();
        final boolean isDocker = DockerLayoutProvider.ALIAS.equalsIgnoreCase(repository.getLayout());
        FutureTask<String> futureTask = null;
        for (List<RepositoryPath> fileList : fileLists) {
            futureTask = new FutureTask<String>(() -> {
                String artifactPath;
                for (RepositoryPath repositoryPath : fileList) {
                    try {
                        artifactPath = RepositoryFiles.relativizePath(repositoryPath);
                        if (!RepositoryFiles.isArtifact(repositoryPath)) {
                            log.info("HandleArtifacts path [{}] not is a artifact", repositoryPath.toString());
                            continue;
                        }
                        if (isDocker) {
                            List<ImageManifest> imageManifestList = dockerComponent.getImageManifests(repositoryPath);
                            if (CollectionUtils.isNotEmpty(imageManifestList)) {
                                for (ImageManifest manifest : imageManifestList) {
                                    List<String> layerList = promotionUtil.getAllLayerList(manifest);
                                    //blobs
                                    for (String layer : layerList) {
                                        RepositoryPath blobPath = repositoryPathResolver.resolve(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), DockerLayoutProvider.BLOBS + File.separator + layer);
                                        if (Boolean.TRUE.equals(force)) {
                                            doForceDelete(blobPath);
                                        }
                                        artifactManagementService.validateAndStoreIndex(blobPath);
                                    }
                                    if (StringUtils.isNotBlank(manifest.getDigest())) {
                                        RepositoryPath manifestPath = repositoryPathResolver.resolve(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), DockerLayoutProvider.MANIFEST + File.separator + manifest.getDigest());
                                        if (Boolean.TRUE.equals(force)) {
                                            doForceDelete(manifestPath);
                                        }
                                        dockerComponent.handleLayers(manifestPath);
                                        artifactManagementService.validateAndStoreIndex(manifestPath);
                                    }
                                }
                            }
                        }
                        if (Boolean.TRUE.equals(force)) {
                            doForceDelete(repositoryPath);
                        }
                        if (Boolean.TRUE.equals(metadata)) {
                            handlerMetadata(artifactPath, repositoryPath);
                        }
                        artifactManagementService.validateAndStoreIndex(repositoryPath);
                        if (Maven2LayoutProvider.ALIAS.equals(repository.getLayout())) {
                            try {
                                artifactMetadataService.rebuildMetadata(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), artifactPath);
                            } catch (Exception ex) {
                                log.error("HandleArtifacts rebuildMetadata path [{}] error [{}]", repositoryPath.toString(), ExceptionUtils.getStackTrace(ex));
                            }
                        }
                    } catch (Exception ex) {
                        log.error("HandleArtifacts path [{}] error [{}]", repositoryPath.toString(), ExceptionUtils.getStackTrace(ex));
                    }
                }
                return "success";
            });
            futureTaskList.add(futureTask);
            asyncThreadPoolTaskExecutor.submit(futureTask);
        }
        for (FutureTask<String> task : futureTaskList) {
            task.get();
        }
        log.info("HandleArtifacts [{}] is finished", rootRepositoryPath.toString());
        return resultList;
    }

    @Override
    public void doForceDelete(RepositoryPath repositoryPath) {
        try {
            //强制构建索引，若图库中存在则删除图库的记录
            Artifact artifact = getArtifact(repositoryPath);
            if (Objects.nonNull(artifact)) {
                //先缓存元数据信息
                artifactComponent.cacheArtifactMetadata(repositoryPath, artifact.getMetadata());
                //删除图库记录
                artifactRepository.delete(artifact, repositoryPath.getRepository().getLayout());
                repositoryPath.setArtifact(null);
                log.info("Delete artifact storageId [{}] repositoryId [{}] path [{}]", artifact.getStorageId(), artifact.getRepositoryId(), artifact.getArtifactPath());
            }
        } catch (Exception ex) {
            log.error("Force delete artifact [{}] error [{}]", repositoryPath.toString(), ExceptionUtils.getStackTrace(ex));
        }
    }

    public boolean hasAdmin() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (Objects.isNull(authentication)) {
            return false;
        }
        SpringSecurityUser userDetails = (SpringSecurityUser) authentication.getPrincipal();
        if (CollectionUtils.isEmpty(userDetails.getRoles())) {
            return false;
        }
        return userDetails.getRoles().stream().anyMatch(item -> SystemRole.ADMIN.name().equals(item.getName()));
    }

    public String loginUsername() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (Objects.isNull(authentication)) {
            return "";
        }
        SpringSecurityUser userDetails = (SpringSecurityUser) authentication.getPrincipal();
        return userDetails.getUsername();
    }

    public List<String> getStorageIdAndRepositoryIdList(List<String> storageIdList) {
        List<String> storageIdAndRepositoryIdList = Lists.newArrayList();
        if (hasAdmin()) {
            List<Storage> storageList = new ArrayList<>(configurationManagementService.getConfiguration().getStorages().values());
            for (Storage storage : storageList) {
                storageIdAndRepositoryIdList.addAll(storage.getRepositories().values().stream().map(item -> String.format("%s-%s", storage.getId(), item.getId())).collect(Collectors.toList()));
            }
            return storageIdAndRepositoryIdList;
        }
        List<Storage> storageList = configurationManagementService.getConfiguration().getStorages().values().stream().filter(item -> storageIdList.contains(item.getId())).collect(Collectors.toList());
        //查询数据库中存储空间绑定的用户
        storageManagementService.getStorageUsers(storageList);

        for (Storage storage : storageList) {
            Set<String> userSet = storage.getUsers();
            if (CollectionUtils.isNotEmpty(userSet) && userSet.contains(loginUsername())) {
                storageIdAndRepositoryIdList.addAll(storage.getRepositories().values().stream().map(item -> String.format("%s-%s", storage.getId(), item.getId())).collect(Collectors.toList()));
            } else if (Objects.nonNull(storage.getRepositories())) {
                storageIdAndRepositoryIdList.addAll(storage.getRepositories().values().stream().filter(item -> RepositoryScopeEnum.OPEN.getType().equals(item.getScope())).map(item -> String.format("%s-%s", storage.getId(), item.getId())).collect(Collectors.toList()));
            }
        }
        return storageIdAndRepositoryIdList;
    }

    private List<String> getStorageIdAndRepositoryId(String storageId, String repositoryId) {
        List<String> storageIdAndRepositoryIdList = null;
        if (StringUtils.isNotBlank(storageId) && StringUtils.isNotBlank(repositoryId)) {
            storageIdAndRepositoryIdList = Collections.singletonList(String.format("%s-%s", storageId, repositoryId));
            Repository repository = configurationManagementService.getMutableConfigurationClone().getStorage(storageId).getRepository(repositoryId);
            boolean isGroupRepository = RepositoryTypeEnum.GROUP.getType().equals(repository.getType());
            if (isGroupRepository) {
                storageIdAndRepositoryIdList = getGroupStorageIdAndRepositoryId(repository);
            }
        }
        return storageIdAndRepositoryIdList;
    }

    private List<String> getGroupStorageIdAndRepositoryId(Repository repository) {
        List<String> storageIdAndRepositoryIdList = Lists.newArrayList();
        List<String> storageAndRepositoryIdList = Lists.newArrayList();
        configurationManager.resolveGroupRepository(repository, storageAndRepositoryIdList);
        for (String storageAndRepositoryId : storageAndRepositoryIdList) {
            String sId = ConfigurationUtils.getStorageId(repository.getStorage().getId(), storageAndRepositoryId);
            String rId = ConfigurationUtils.getRepositoryId(storageAndRepositoryId);
            Repository subRepository = configurationManagementService.getConfiguration().getRepository(sId, rId);
            if (!subRepository.isInService()) {
                continue;
            }
            if (!subRepository.isAllowsDirectoryBrowsing()) {
                continue;
            }
            storageIdAndRepositoryIdList.add(subRepository.getStorage().getId() + "-" + subRepository.getId());
        }
        return storageIdAndRepositoryIdList;
    }

    @Override
    public String handlerMetadata(String artifactPath, RepositoryPath repositoryPath) {
        try {
            Artifact artifact = null;
            String metadata = "";
            String metadataPath = String.format("%s%s/%s", artifactPath, ".artifactory-metadata", "properties.xml");
            RepositoryPath metadataRepositoryPath = repositoryPathResolver.resolve(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), metadataPath);
            if (Objects.nonNull(metadataRepositoryPath) && Files.exists(metadataRepositoryPath)) {
                String metadataXml = Files.readString(metadataRepositoryPath), metadataValue;
                ArtifactMetadata artifactMetadata = null;
                // 创建XML解析器
                XmlMapper xmlMapper = new XmlMapper();
                // 将XML解析为JsonNode对象
                JsonNode jsonNode = xmlMapper.readTree(metadataXml);
                // 使用ObjectMapper将JsonNode转换为JSON字符串
                ObjectMapper objectMapper = new ObjectMapper();
                String metadataJsonStr = objectMapper.writeValueAsString(jsonNode);
                JSONObject metadataJson = JSONObject.parseObject(metadataJsonStr), itemMetadataJson = new JSONObject();
                for (String metadataKey : metadataJson.keySet()) {
                    metadataValue = metadataJson.getString(metadataKey);
                    artifactMetadata = ArtifactMetadata.builder().type(ArtifactMetadataEnum.STRING.toString()).value(metadataValue).viewShow(1).build();
                    itemMetadataJson.put(metadataKey, artifactMetadata);
                }
                metadata = JSONObject.toJSONString(itemMetadataJson);
                promotionUtil.setMetaData(repositoryPath, metadata);
            }
            String fileName = "." + FilenameUtils.getName(repositoryPath.getFileName().toString()) + ".metadata";
            RepositoryPath artifactMetadataRepositoryPath = repositoryPath.getParent().resolve(fileName);
            if (Files.exists(artifactMetadataRepositoryPath)) {
                try (InputStream inputStream = Files.newInputStream(artifactMetadataRepositoryPath);
                     ObjectInputStream objectInputStream = new ObjectInputStream(inputStream)) {
                    artifact = (Artifact) objectInputStream.readObject();
                    if (Objects.nonNull(artifact) && StringUtils.isNotBlank(artifact.getMetadata())) {
                        metadata = artifact.getMetadata();
                        promotionUtil.setMetaData(repositoryPath, metadata);
                    }
                } catch (Exception ex) {
                    Files.deleteIfExists(artifactMetadataRepositoryPath);
                    log.debug("解析制品 [{}] 本地缓存.metadata文件错误", ExceptionUtils.getStackTrace(ex));
                }
            }
            //从元数据缓存文件中获取元数据
            metadata = artifactComponent.getCacheArtifactMetadata(repositoryPath);
            if (StringUtils.isNotBlank(metadata)) {
                promotionUtil.setMetaData(repositoryPath, metadata);
            }
            log.info("Artifact storageId [{}] repositoryId [{}] path [{}] metadata [{}]", repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), artifactPath, metadata);
            return metadata;
        } catch (Exception ex) {
            log.error("handleArtifact sync metadata path：{}，error：{}", repositoryPath.toString(), ExceptionUtils.getStackTrace(ex));
        }
        return "";
    }

    private Repository getRepository(String storageId, String repositoryId) {
        return configurationManagementService.getConfiguration().getRepository(storageId, repositoryId);
    }

    private void validateAuth(RepositoryPath repositoryPath) {
        if (!hasAuth(repositoryPath)) {
            throw new BusinessException("没有操作元数据权限");
        }
    }

    private boolean hasAuth(RepositoryPath repositoryPath) {
        try {
            String threadName = Thread.currentThread().getName();
            List<String> ignoreThreadNameList = GlobalConstants.IGNORE_THREAD_NAME_LIST;
            if (ignoreThreadNameList.stream().anyMatch(threadName::startsWith)) {
                return true;
            }
            return authComponent.validatePrivileges(repositoryPath.getRepository(), repositoryPath, Privileges.CONFIGURATION_ADD_UPDATE_METADATA.getAuthority());
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
        return false;
    }

    private void cacheMetadata(RepositoryPath repositoryPath) {
        artifactComponent.storeArtifactMetadataFile(repositoryPath);
    }

    private void saveOrUpdateArtifactMetadata(RepositoryPath repositoryPath, Artifact artifact, ArtifactMetadataForm artifactMetadataForm) throws IOException {
        JSONObject metadataJson = getMetadata(artifact);
        metadataJson = metadataJson == null ? new JSONObject() : metadataJson;
        ArtifactMetadata artifactMetadata = ArtifactMetadata.builder().build();
        BeanUtils.copyProperties(artifactMetadataForm, artifactMetadata);
        metadataJson.put(artifactMetadataForm.getKey(), artifactMetadata);
        artifact.setMetadata(metadataJson.toJSONString());
        artifactService.saveOrUpdateArtifact(artifact);
        repositoryPath.setArtifact(artifact);
        artifactEvent.dispatchArtifactMetaDataEvent(repositoryPath);
        cacheMetadata(repositoryPath);
    }

    private void saveOrUpdateDirectoryMetadata(RepositoryPath repositoryPath, ArtifactMetadataForm artifactMetadataForm) {
        String metadata = artifactComponent.getCacheArtifactMetadata(repositoryPath);
        JSONObject metadataJson = StringUtils.isNotBlank(metadata) ? JSONObject.parseObject(metadata) : new JSONObject();
        ArtifactMetadata artifactMetadata = ArtifactMetadata.builder().build();
        BeanUtils.copyProperties(artifactMetadataForm, artifactMetadata);
        metadataJson.put(artifactMetadataForm.getKey(), artifactMetadata);
        artifactComponent.cacheArtifactMetadata(repositoryPath, metadataJson.toJSONString());
    }

    private void recursiveMetadata(RepositoryPath repositoryPath, ArtifactMetadataForm artifactMetaData) {
        //目录级别元数据
        Boolean recursive = artifactMetaData.getRecursive();
        if (Objects.isNull(recursive) || Boolean.FALSE.equals(recursive)) {
            saveOrUpdateDirectoryMetadata(repositoryPath, artifactMetaData);
            return;
        }
        try {
            String layout = repositoryPath.getRepository().getLayout();
            final boolean isDockerLayout = ProductTypeEnum.Docker.getFoLibraryName().equalsIgnoreCase(layout);
            Files.walkFileTree(repositoryPath, new SimpleFileVisitor<Path>() {
                @Override
                public FileVisitResult visitFileFailed(Path file, IOException exc) throws IOException {
                    if (exc instanceof NoSuchFileException) {
                        // 目录或文件已删除，继续遍历
                        return FileVisitResult.CONTINUE;
                    }
                    return super.visitFileFailed(file, exc);
                }

                @Override
                public FileVisitResult visitFile(Path file,
                                                 BasicFileAttributes attrs)
                        throws IOException {
                    RepositoryPath itemPath = (RepositoryPath) file;
                    try {
                        if (RepositoryPathUtil.include(1, itemPath, isDockerLayout, layout)) {
                            log.info("Find path [{}]", itemPath);
                            Artifact artifact = resolvePath(itemPath.getStorageId(), itemPath.getRepositoryId(), RepositoryFiles.relativizePath(itemPath));
                            if (Objects.isNull(artifact)) {
                                return FileVisitResult.CONTINUE;
                            }
                            saveOrUpdateArtifactMetadata(itemPath, artifact, artifactMetaData);
                        }
                    } catch (Exception ex) {
                        log.error("Find path [{}] metadata error [{}]", itemPath, ExceptionUtils.getStackTrace(ex));
                        return FileVisitResult.CONTINUE;
                    }
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult preVisitDirectory(final Path dir, final BasicFileAttributes attrs) throws IOException {
                    try {
                        RepositoryPath itemPath = (RepositoryPath) dir;
                        if (!Files.isSameFile(itemPath, itemPath.getRoot()) && !RepositoryPathUtil.include(2, itemPath, isDockerLayout, layout) || (CollectionUtils.isNotEmpty(RepositoryPathUtil.EXCLUDE_LIST) && RepositoryPathUtil.EXCLUDE_LIST.stream().anyMatch(item -> itemPath.getFileName().toString().equalsIgnoreCase(item)))) {
                            log.info("RepositoryPath [{}] skip...", itemPath.toString());
                            return FileVisitResult.SKIP_SUBTREE;
                        }
                        saveOrUpdateDirectoryMetadata(itemPath, artifactMetaData);
                    } catch (NoSuchFileException e) {
                        // 文件已删除，跳过处理
                        return FileVisitResult.CONTINUE;
                    }
                    return FileVisitResult.CONTINUE;
                }
            });
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException(ex);
        }
    }

    private void saveOrUpdateArtifactBatchMetadata(RepositoryPath repositoryPath, Artifact artifact, List<ArtifactMetadataForm> artifactMetadataFormList) throws IOException {
        JSONObject metadataJson = getMetadata(artifact);
        metadataJson = metadataJson == null ? new JSONObject() : metadataJson;
        for (ArtifactMetadataForm artifactMetadataForm : artifactMetadataFormList) {
            String key = artifactMetadataForm.getKey();
            ArtifactMetadata artifactMetadata = ArtifactMetadata.builder().build();
            BeanUtils.copyProperties(artifactMetadataForm, artifactMetadata);
            metadataJson.put(key, artifactMetadata);
        }
        artifact.setMetadata(metadataJson.toJSONString());
        artifactService.saveOrUpdateArtifact(artifact);
        repositoryPath.setArtifact(artifact);
        artifactEvent.dispatchArtifactMetaDataEvent(repositoryPath);
        cacheMetadata(repositoryPath);
    }

    private void saveOrUpdateDirectoryBatchMetadata(RepositoryPath repositoryPath, List<ArtifactMetadataForm> artifactMetadataFormList) {
        String metadata = artifactComponent.getCacheArtifactMetadata(repositoryPath);
        JSONObject metadataJson = StringUtils.isNotBlank(metadata) ? JSONObject.parseObject(metadata) : new JSONObject();
        for (ArtifactMetadataForm artifactMetadataForm : artifactMetadataFormList) {
            String key = artifactMetadataForm.getKey();
            ArtifactMetadata artifactMetadata = ArtifactMetadata.builder().build();
            BeanUtils.copyProperties(artifactMetadataForm, artifactMetadata);
            metadataJson.put(key, artifactMetadata);
        }
        artifactComponent.cacheArtifactMetadata(repositoryPath, metadataJson.toJSONString());
    }

    private void recursiveBatchMetadata(RepositoryPath repositoryPath, ArtifactMetadataForm artifactMetaData, List<ArtifactMetadataForm> artifactMetadataFormList) {
        //目录级别元数据
        Boolean recursive = artifactMetaData.getRecursive();
        if (Objects.isNull(recursive) || Boolean.FALSE.equals(recursive)) {
            saveOrUpdateDirectoryBatchMetadata(repositoryPath, artifactMetadataFormList);
            return;
        }
        try {
            String layout = repositoryPath.getRepository().getLayout();
            final boolean isDockerLayout = ProductTypeEnum.Docker.getFoLibraryName().equalsIgnoreCase(layout);
            Files.walkFileTree(repositoryPath, new SimpleFileVisitor<Path>() {
                @Override
                public FileVisitResult visitFileFailed(Path file, IOException exc) throws IOException {
                    if (exc instanceof NoSuchFileException) {
                        // 目录或文件已删除，继续遍历
                        return FileVisitResult.CONTINUE;
                    }
                    return super.visitFileFailed(file, exc);
                }

                @Override
                public FileVisitResult visitFile(Path file,
                                                 BasicFileAttributes attrs)
                        throws IOException {
                    RepositoryPath itemPath = (RepositoryPath) file;
                    try {
                        if (RepositoryPathUtil.include(1, itemPath, isDockerLayout, layout)) {
                            log.info("Find path [{}]", itemPath);
                            Artifact artifact = resolvePath(itemPath.getStorageId(), itemPath.getRepositoryId(), RepositoryFiles.relativizePath(itemPath));
                            if (Objects.isNull(artifact)) {
                                return FileVisitResult.CONTINUE;
                            }
                            saveOrUpdateArtifactBatchMetadata(itemPath, artifact, artifactMetadataFormList);
                        }
                    } catch (Exception ex) {
                        log.error("Find path [{}] metadata error [{}]", itemPath, ExceptionUtils.getStackTrace(ex));
                        return FileVisitResult.CONTINUE;
                    }
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult preVisitDirectory(final Path dir, final BasicFileAttributes attrs) throws IOException {
                    try {
                        RepositoryPath itemPath = (RepositoryPath) dir;
                        if (!Files.isSameFile(itemPath, itemPath.getRoot()) && !RepositoryPathUtil.include(2, itemPath, isDockerLayout, layout) || (CollectionUtils.isNotEmpty(RepositoryPathUtil.EXCLUDE_LIST) && RepositoryPathUtil.EXCLUDE_LIST.stream().anyMatch(item -> itemPath.getFileName().toString().equalsIgnoreCase(item)))) {
                            log.info("RepositoryPath [{}] skip...", itemPath.toString());
                            return FileVisitResult.SKIP_SUBTREE;
                        }
                        saveOrUpdateDirectoryBatchMetadata(itemPath, artifactMetadataFormList);
                    } catch (NoSuchFileException e) {
                        // 文件已删除，跳过处理
                        return FileVisitResult.CONTINUE;
                    }
                    return FileVisitResult.CONTINUE;
                }
            });
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException(ex);
        }
    }

    private void deleteArtifactMetadata(RepositoryPath repositoryPath, Artifact artifact, ArtifactMetadataForm artifactMetadataForm) throws IOException {
        JSONObject metadataJson = getMetadata(artifact);
        if (Objects.nonNull(metadataJson) && metadataJson.containsKey(artifactMetadataForm.getKey())) {
            validateAuth(repositoryPath);
            metadataJson.remove(artifactMetadataForm.getKey());
            artifact.setMetadata(metadataJson.toJSONString());
            artifactService.saveOrUpdateArtifact(artifact);
            repositoryPath.setArtifact(artifact);
            artifactEvent.dispatchArtifactMetaDataEvent(repositoryPath);
            cacheMetadata(repositoryPath);
        }
    }

    private void deleteDirectoryMetadata(RepositoryPath repositoryPath, ArtifactMetadataForm artifactMetadataForm) {
        String metadata = artifactComponent.getCacheArtifactMetadata(repositoryPath);
        if (StringUtils.isBlank(metadata)) {
            return;
        }
        JSONObject metadataJson = StringUtils.isNotBlank(metadata) ? JSONObject.parseObject(metadata) : new JSONObject();
        metadataJson.remove(artifactMetadataForm.getKey());
        artifactComponent.cacheArtifactMetadata(repositoryPath, metadataJson.toJSONString());
    }

    private void recursiveDeleteMetadata(RepositoryPath repositoryPath, ArtifactMetadataForm artifactMetaData) {
        //目录级别元数据
        Boolean recursive = artifactMetaData.getRecursive();
        if (Objects.isNull(recursive) || Boolean.FALSE.equals(recursive)) {
            deleteDirectoryMetadata(repositoryPath, artifactMetaData);
            return;
        }
        try {
            String layout = repositoryPath.getRepository().getLayout();
            final boolean isDockerLayout = ProductTypeEnum.Docker.getFoLibraryName().equalsIgnoreCase(layout);
            Files.walkFileTree(repositoryPath, new SimpleFileVisitor<Path>() {
                @Override
                public FileVisitResult visitFileFailed(Path file, IOException exc) throws IOException {
                    if (exc instanceof NoSuchFileException) {
                        // 目录或文件已删除，继续遍历
                        return FileVisitResult.CONTINUE;
                    }
                    return super.visitFileFailed(file, exc);
                }

                @Override
                public FileVisitResult visitFile(Path file,
                                                 BasicFileAttributes attrs)
                        throws IOException {
                    RepositoryPath itemPath = (RepositoryPath) file;
                    try {
                        if (RepositoryPathUtil.include(1, itemPath, isDockerLayout, layout)) {
                            log.info("Find path [{}]", itemPath);
                            Artifact artifact = resolvePath(itemPath.getStorageId(), itemPath.getRepositoryId(), RepositoryFiles.relativizePath(itemPath));
                            if (Objects.isNull(artifact)) {
                                return FileVisitResult.CONTINUE;
                            }
                            deleteArtifactMetadata(itemPath, artifact, artifactMetaData);
                        }
                    } catch (Exception ex) {
                        log.error("Find path [{}] metadata error [{}]", itemPath, ExceptionUtils.getStackTrace(ex));
                        return FileVisitResult.CONTINUE;
                    }
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult preVisitDirectory(final Path dir, final BasicFileAttributes attrs) throws IOException {
                    try {
                        RepositoryPath itemPath = (RepositoryPath) dir;
                        if (!Files.isSameFile(itemPath, itemPath.getRoot()) && !RepositoryPathUtil.include(2, itemPath, isDockerLayout, layout) || (CollectionUtils.isNotEmpty(RepositoryPathUtil.EXCLUDE_LIST) && RepositoryPathUtil.EXCLUDE_LIST.stream().anyMatch(item -> itemPath.getFileName().toString().equalsIgnoreCase(item)))) {
                            log.info("RepositoryPath [{}] skip...", itemPath.toString());
                            return FileVisitResult.SKIP_SUBTREE;
                        }
                        deleteDirectoryMetadata(itemPath, artifactMetaData);
                    } catch (NoSuchFileException e) {
                        // 文件已删除，跳过处理
                        return FileVisitResult.CONTINUE;
                    }
                    return FileVisitResult.CONTINUE;
                }
            });
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException(ex);
        }
    }


    @Override
    public List<DockeerImageResult> queryDockerImages(Integer numImages, long imageSize) {
        //获取docker 镜像列表
        DockerCatalog dockerCatalog = getDockerCatalog(numImages,  null);
        if(Objects.isNull(dockerCatalog) || dockerCatalog.getRepositories().isEmpty()){
            return Collections.emptyList();
        }
        List<DockeerImageResult> imageResults = new ArrayList<>();
        for(String name : dockerCatalog.getRepositories()){
            List<DockeerImageResult> results = getDockerTsags(null,  null, name);
            if(results != null && !results.isEmpty()){
                imageResults.addAll(results);
            }
        }
        BigDecimal totalSize = BigDecimal.valueOf(imageSize);
        return imageResults.stream().filter(item -> item.getSize().compareTo(totalSize)>=0).collect(Collectors.toList());
    }

    public long getDockerImageSize(String storageId, String repositoryId, String path) throws IOException {

        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
        DirectoryListing directoryListing = directoryListingService.fromRepositoryPath(repositoryPath);
        List<FileContent> fileContents = directoryListing.getFiles().stream().filter(file -> DockerCoordinates.include(file.getName())).collect(Collectors.toList());
        FileContent fileContent = fileContents.get(0);
        RepositoryPath versionPath = repositoryPathResolver.resolve(storageId, repositoryId, path + File.separator + fileContent.getName());

        String manifestString = Files.readString(versionPath);
        ImageManifest imageManifest = JSON.parseObject(manifestString, ImageManifest.class);
        if (CollectionUtils.isNotEmpty(imageManifest.getManifests())) {
            Manifests manifests = imageManifest.getManifests().get(0);
            RepositoryPath manifestPath = repositoryPathResolver.resolve(storageId, repositoryId, "manifest/" + manifests.getDigest());
            if (Objects.nonNull(manifestPath) && Files.exists(manifestPath)) {
                ImageManifest manifest = JSON.parseObject(Files.readString(manifestPath), ImageManifest.class);
                if (Objects.nonNull(manifest)) {
                    List<Manifests> manifestsList = imageManifest.getManifests();
                    imageManifest = manifest;
                    imageManifest.setManifests(manifestsList);
                }
            }
        }
        return Optional.ofNullable(imageManifest.getLayers()).orElse(Collections.emptyList()).stream().filter(item -> Objects.nonNull(item.getSize())).mapToLong(LayerManifest::getSize).sum();
    }

   public DockerCatalog  getDockerCatalog(Integer n , String last){
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        try {
            log.info("GET Catalog n [{}] last [{}]", n, last);
            List<Storage> storageList = new ArrayList<>(configurationManagementService.getConfiguration()
                    .getStorages()
                    .values());
            String username = "";
            if (Objects.nonNull(authentication)) {
                final UserDetails loggedUser = (UserDetails) authentication.getPrincipal();
                username = loggedUser.getUsername();
            }
            String link = "", next = "";
            List<String> resultList = Collections.emptyList(), dataList = Lists.newArrayList();
            if (CollectionUtil.isNotEmpty(storageList)) {
                //查询数据库中存储空间绑定的用户
                storageManagementService.getStorageUsers(storageList);
                boolean filterByUser = !hasAdmin();
                String finalUsername = username;
                storageList = storageList.stream()
                        .distinct()
                        .filter(s -> !filterByUser || (CollectionUtil.isNotEmpty(s.getUsers()) && s.getUsers().contains(finalUsername)) ||
                                (CollectionUtils.isNotEmpty(s.getRepositories().values()) && s.getRepositories().values().stream().anyMatch(repository -> RepositoryScopeEnum.OPEN.getType().equals(repository.getScope()))))
                        .collect(Collectors.toCollection(LinkedList::new));
                List<Repository> repositories;
                String dockerLevel = System.getProperty("DockerLevel");
                for (Storage storage : storageList) {
                    boolean flag = !hasAdmin() && !username.equals(storage.getAdmin()) && (CollectionUtils.isNotEmpty(storage.getUsers()) && !storage.getUsers().contains(username));
                    repositories = new LinkedList<Repository>(storage.getRepositories().values());
                    repositories = repositories.stream().distinct()
                            .filter(r -> DockerLayoutProvider.ALIAS.equalsIgnoreCase(r.getLayout()))
                            .collect(Collectors.toCollection(LinkedList::new));
                    if (flag) {
                        repositories = repositories.stream().filter((item -> RepositoryScopeEnum.OPEN.getType().equals(item.getScope()))).collect(Collectors.toList());
                    }
                    if (CollectionUtils.isNotEmpty(repositories)) {
                        repositories.forEach(repository -> {
                            try {
                                RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository.getStorage().getId(), repository.getId());
                                String prefix = String.format("%s/%s", repository.getStorage().getId(), repository.getId());
                                if (Objects.nonNull(repositoryPath) && Files.exists(repositoryPath)) {
                                    if (GlobalConstants.DOCKER_LEVEL_SINGLE.equals(dockerLevel)) {
                                        DirectoryListing directoryListing = directoryListingService.fromRepositoryPath(repositoryPath);
                                        if (Objects.nonNull(directoryListing) && CollectionUtils.isNotEmpty(directoryListing.getDirectories())) {
                                            dataList.addAll(directoryListing.getDirectories().stream().filter(item -> StringUtils.isNotBlank(item.getName()) && !item.getName().startsWith(".")).map(item -> String.format("%s/%s", prefix, item.getName())).collect(Collectors.toList()));
                                        }
                                    } else {
                                        List<RepositoryPath> repositoryPathList = RepositoryPathUtil.getDockerImagePaths(repositoryPath);
                                        if (CollectionUtils.isNotEmpty(repositoryPathList)) {
                                            repositoryPathList.forEach(item -> {
                                                String path = "";
                                                try {
                                                    path = RepositoryFiles.relativizePath(item);
                                                } catch (Exception ex) {
                                                    log.error(ExceptionUtils.getStackTrace(ex));
                                                }
                                                if (StringUtils.isNotBlank(path)) {
                                                    dataList.add(String.format("%s/%s", prefix, path));
                                                }
                                            });
                                        }
                                    }
                                }
                            } catch (Exception ex) {
                                log.error("GET Catalog directory listing error [{}]", ExceptionUtils.getStackTrace(ex));
                            }
                        });
                    }
                }
                int size = dataList.size(), startIndex = 0, endIndex = size;
                if (StringUtils.isNotBlank(last)) {
                    int index = dataList.indexOf(last);
                    if (index != -1) {
                        startIndex = index + 1;
                    }
                }
                if (startIndex > size) {
                    startIndex = size;
                }
                if (Objects.nonNull(n)) {
                    if (n < 1) {
                        n = size;
                    }
                    endIndex = startIndex + n;
                }
                if (endIndex > size) {
                    endIndex = size;
                }
                resultList = dataList.subList(startIndex, endIndex);

                log.info("GET Catalog n [{}] last [{}] startIndex [{}] endIndex [{}] link [{}]", n, last, startIndex, endIndex, link);
            }
            DockerCatalog dockerCatalog = DockerCatalog.builder().next(next).repositories(resultList).build();

            return dockerCatalog;
        } catch (Exception ex) {
            log.error("GET Catalog [n:{}, last:{} error {}]", n, last, ExceptionUtils.getStackTrace(ex));

        }
        return null;
    }


    public List<DockeerImageResult> getDockerTsags(Integer n , String last,String path ) {
        // 1. 去除首尾斜杠，避免空元素
        String trimmed = path.replaceAll("^/+|/+$", "");

        // 2. 按斜杠分割
        String[] parts = trimmed.split("/");

        // 3. 校验分段数量
        if (parts.length < 3) {
            throw new IllegalArgumentException("路径格式错误，path:"+path);
        }

        // 4. 提取各段
        String storageId = parts[0];
        String repositoryId = parts[1];
        String imagePath =String.join("/", Arrays.copyOfRange(parts, 2, parts.length));

        try {
            log.info("Listing Image Tags storageId [{}] repositoryId [{}] imagePath [{}] n [{}] last [{}]", storageId, repositoryId, imagePath, n, last);
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, imagePath);
            List<FileContent> tagDirList = null;
            if (Files.exists(repositoryPath)) {
                DirectoryListing directoryListing = directoryListingService.fromRepositoryPath(repositoryPath);
                tagDirList = Optional.ofNullable(directoryListing.getDirectories()).orElse(Collections.emptyList()).stream().filter(item -> RepositoryPathUtil.isDockerTag(repositoryPathResolver.resolve(storageId, repositoryId, item.getArtifactPath()))).collect(Collectors.toList());
            }
            List<String> tagList = Optional.ofNullable(tagDirList).orElse(Collections.emptyList()).stream().map(FileContent::getName).collect(Collectors.toList());
            List<String> resultList;
            int size = tagList.size(), startIndex = 0, endIndex = size;
            if (StringUtils.isNotBlank(last)) {
                int index = tagList.indexOf(last);
                if (index != -1) {
                    startIndex = index + 1;
                }
            }
            if (startIndex > size) {
                startIndex = size;
            }
            if (Objects.nonNull(n)) {
                if (n < 1) {
                    n = size;
                }
                endIndex = startIndex + n;
            }
            if (endIndex > size) {
                endIndex = size;
            }
            resultList = tagList.subList(startIndex, endIndex);
            DockerTags dockerTags = DockerTags.builder().name(imagePath).tags(resultList).build();

            if(dockerTags !=null && !dockerTags.getTags().isEmpty()){
                List<DockeerImageResult> results  = new ArrayList<>(dockerTags.getTags().size());
                for (String tag : dockerTags.getTags()){
                    DockeerImageResult dockeerImageResult = new DockeerImageResult();
                    dockeerImageResult.setArtifactName(dockerTags.getName());
                    dockeerImageResult.setPath(String.format("%s/%s/%s:%s", storageId,repositoryId,dockerTags.getName(),tag));
                    dockeerImageResult.setStorageId(storageId);
                    dockeerImageResult.setTag(tag);
                    dockeerImageResult.setRepositoryId(repositoryId);
                    long  imageSize = getDockerImageSize(storageId, repositoryId,imagePath+"/"+tag);
                    dockeerImageResult.setSize(FileSizeConvertUtils.convertBytesWithDecimal(imageSize, "MB"));
                    results.add(dockeerImageResult);
                }
                return results;
            }else {
                return null;
            }

        } catch (Exception ex) {
            log.error("Listing Image Tags storageId [{}] repositoryId [{}] imagePath [{}] n [{}] last [{}] error [{}]", storageId, repositoryId, imagePath, n, last, ExceptionUtils.getStackTrace(ex));
        }
        return null;
    }

}
