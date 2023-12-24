package com.veadan.folib.services.impl;

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
import com.alibaba.fastjson.JSONObject;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.xml.XmlMapper;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.sun.management.HotSpotDiagnosticMXBean;
import com.veadan.folib.artifact.coordinates.DockerArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.MavenArtifactCoordinates;
import com.veadan.folib.authorization.dto.Role;
import com.veadan.folib.cloud.storage.s3fs.util.UriUtils;
import com.veadan.folib.cluster.SyncMetadataEnum;
import com.veadan.folib.components.artifact.ArtifactComponent;
import com.veadan.folib.components.layout.DockerComponent;
import com.veadan.folib.configuration.ConfigurationUtils;
import com.veadan.folib.configuration.MutableMetadataConfiguration;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.controllers.ResponseMessage;
import com.veadan.folib.controllers.cluster.dto.SyncMetadataDto;
import com.veadan.folib.domain.*;
import com.veadan.folib.domain.thirdparty.ArtifactInfo;
import com.veadan.folib.domain.thirdparty.ArtifactQuery;
import com.veadan.folib.entity.Dict;
import com.veadan.folib.enums.ArtifactMetadataEnum;
import com.veadan.folib.enums.DictTypeEnum;
import com.veadan.folib.enums.RepositoryScopeEnum;
import com.veadan.folib.event.artifact.ArtifactEventListenerRegistry;
import com.veadan.folib.forms.artifact.ArtifactMetadataForm;
import com.veadan.folib.forms.dict.DictForm;
import com.veadan.folib.forms.scanner.*;
import com.veadan.folib.gremlin.dsl.EntityTraversalUtils;
import com.veadan.folib.gremlin.entity.vo.ArtifactVo;
import com.veadan.folib.promotion.PromotionUtil;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.io.RootRepositoryPath;
import com.veadan.folib.providers.layout.DockerLayoutProvider;
import com.veadan.folib.providers.layout.LayoutProvider;
import com.veadan.folib.providers.layout.LayoutProviderRegistry;
import com.veadan.folib.providers.layout.Maven2LayoutProvider;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.scanner.common.msg.TableResultResponse;
import com.veadan.folib.scanner.entity.ScanRules;
import com.veadan.folib.scanner.mapper.ScanRulesMapper;
import com.veadan.folib.scanner.service.ScanService;
import com.veadan.folib.schema2.ImageManifest;
import com.veadan.folib.services.*;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.StorageDto;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import com.veadan.folib.users.domain.SystemRole;
import com.veadan.folib.users.userdetails.SpringSecurityUser;
import com.veadan.folib.util.CompressUtils;
import com.veadan.folib.util.CustomDateUtils;
import com.veadan.folib.util.FileSizeConvertUtils;
import com.veadan.folib.util.RepositoryPathUtil;
import com.veadan.folib.utils.TreeUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
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
import org.springframework.web.multipart.commons.CommonsMultipartFile;
import org.springframework.web.util.UriComponentsBuilder;
import tk.mybatis.mapper.entity.Example;

import javax.inject.Inject;
import javax.servlet.http.HttpServletResponse;
import javax.transaction.Transactional;
import java.io.*;
import java.lang.management.ManagementFactory;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.*;
import java.util.concurrent.FutureTask;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Slf4j
@Service
@Transactional
public class ArtifactWebServiceImpl implements ArtifactWebService {

    @Inject
    private ArtifactRepository artifactRepository;

    @Inject
    private ArtifactService artifactService;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private ConfigurationManagementService configurationManagementService;

    @Inject
    private ClusterSyncService clusterSyncService;

    @Inject
    private ScanRulesMapper scanRulesMapper;

    @Inject
    @Qualifier("browseRepositoryDirectoryListingService")
    private DirectoryListingService directoryListingService;

    @Autowired
    private ArtifactEventListenerRegistry artifactEvent;

    @Inject
    private ArtifactManagementService artifactManagementService;

    @Inject
    private ThreadPoolTaskExecutor asyncThreadPoolTaskExecutor;

    @Inject
    @Lazy
    private ScanService scanService;

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

    @Value("${folib.temp}")
    private String tempPath;

    @Override
    public void exportExcel(String vulnerabilityUuid, String storageId, String repositoryId, HttpServletResponse response) throws IOException {
        List<String> storageIdAndRepositoryIdList = getStorageIdAndRepositoryId(storageId, repositoryId);
        List<Artifact> artifactList = artifactRepository.findMatchingByVulnerabilityUuid(vulnerabilityUuid, null, storageIdAndRepositoryIdList);
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
    public void globalSettingAddOrUpdateMetadata(ArtifactMetadataForm artifactMetadataForm) throws IOException {
        MutableMetadataConfiguration mutableMetadataConfiguration = MutableMetadataConfiguration.builder().build();
        BeanUtils.copyProperties(artifactMetadataForm, mutableMetadataConfiguration);
        configurationManagementService.addOrUpdateMetadataConfiguration(mutableMetadataConfiguration);
        //向其他节点同步
        syncDataMetadataConfiguration(mutableMetadataConfiguration, SyncMetadataEnum.ADD_OR_UPDATE);
    }

    @Override
    public void globalSettingDeleteMetadata(ArtifactMetadataForm artifactMetadataForm) throws IOException {
        configurationManagementService.deleteMetadataConfig(artifactMetadataForm.getKey());
        //向其他节点同步
        syncDataMetadataConfiguration(MutableMetadataConfiguration.builder().key(artifactMetadataForm.getKey()).build(), SyncMetadataEnum.DELETE);
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
        try {
            Artifact artifact = resolvePath(artifactMetadataForm.getStorageId(), artifactMetadataForm.getRepositoryId(), artifactMetadataForm.getArtifactPath());
            if (Objects.isNull(artifact)) {
                throw new RuntimeException(GlobalConstants.ARTIFACT_NOT_FOUND_MESSAGE);
            }
            JSONObject metadataJson = getMetadata(artifact);
            if (Objects.isNull(metadataJson)) {
                metadataJson = new JSONObject();
            }
            String key = artifactMetadataForm.getKey();
            if (metadataJson.containsKey(key)) {
                //已存在
                return "repeat";
            }
            ArtifactMetadata artifactMetadata = ArtifactMetadata.builder().build();
            BeanUtils.copyProperties(artifactMetadataForm, artifactMetadata);
            metadataJson.put(key, artifactMetadata);
            artifact.setMetadata(metadataJson.toJSONString());
            artifactService.saveOrUpdateArtifact(artifact);
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(artifactMetadataForm.getStorageId(), artifactMetadataForm.getRepositoryId(), artifactMetadataForm.getArtifactPath());
            repositoryPath.setArtifact(artifact);
            artifactEvent.dispatchArtifactMetaDataEvent(repositoryPath);
        } catch (Exception ex) {
            log.error("保存制品元数据错误：{}", ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException("保存制品元数据错误，请稍后重试");
        }
        return ResponseMessage.ok().getMessage();
    }

    @Override
    public String updateArtifactMetadata(ArtifactMetadataForm artifactMetadataForm) {
        try {
            Artifact artifact = resolvePath(artifactMetadataForm.getStorageId(), artifactMetadataForm.getRepositoryId(), artifactMetadataForm.getArtifactPath());
            JSONObject metadataJson = getMetadata(artifact);
            String key = artifactMetadataForm.getKey();
            metadataJson = metadataJson == null ? new JSONObject() : metadataJson;
            if (metadataJson.containsKey(key)) {
                ArtifactMetadata artifactMetadata = ArtifactMetadata.builder().build();
                BeanUtils.copyProperties(artifactMetadataForm, artifactMetadata);
                metadataJson.put(key, artifactMetadata);
                artifact.setMetadata(metadataJson.toJSONString());
                artifactService.saveOrUpdateArtifact(artifact);
                RepositoryPath repositoryPath = repositoryPathResolver.resolve(artifactMetadataForm.getStorageId(), artifactMetadataForm.getRepositoryId(), artifactMetadataForm.getArtifactPath());
                repositoryPath.setArtifact(artifact);
                artifactEvent.dispatchArtifactMetaDataEvent(repositoryPath);
            }
        } catch (Exception ex) {
            log.error("修改制品元数据错误：{}", ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException("修改制品元数据错误，请稍后重试");
        }
        return ResponseMessage.ok().getMessage();
    }

    @Override
    public void deleteArtifactMetadata(ArtifactMetadataForm artifactMetadataForm) {
        try {
            Artifact artifact = resolvePath(artifactMetadataForm.getStorageId(), artifactMetadataForm.getRepositoryId(), artifactMetadataForm.getArtifactPath());
            JSONObject metadataJson = getMetadata(artifact);
            if (Objects.nonNull(metadataJson) && metadataJson.containsKey(artifactMetadataForm.getKey())) {
                metadataJson.remove(artifactMetadataForm.getKey());
                artifact.setMetadata(metadataJson.toJSONString());
                artifactService.saveOrUpdateArtifact(artifact);
                RepositoryPath repositoryPath = repositoryPathResolver.resolve(artifactMetadataForm.getStorageId(), artifactMetadataForm.getRepositoryId(), artifactMetadataForm.getArtifactPath());
                repositoryPath.setArtifact(artifact);
                artifactEvent.dispatchArtifactMetaDataEvent(repositoryPath);
            }
        } catch (Exception ex) {
            log.error("删除制品元数据错误：{}", ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException("删除制品元数据错误，请稍后重试");
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
        Example example = new Example(ScanRules.class);
        example.createCriteria().andEqualTo("onScan", 1).andIn("id", storageIdAndRepositoryIdList);
        List<ScanRules> scanRulesList = scanRulesMapper.selectByExample(example);
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
                DockerArtifactCoordinates dockerArtifactCoordinates = (DockerArtifactCoordinates) artifact.getArtifactCoordinates();
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
            // 查询是否存在 path 的更新事件 todo
            Artifact artifact = null;
            try {

                artifact = resolvePath(artifactMetaData.getStorageId(), artifactMetaData.getRepositoryId(), artifactMetaData.getArtifactPath());
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
                RepositoryPath repositoryPath = repositoryPathResolver.resolve(artifactMetaData.getStorageId(), artifactMetaData.getRepositoryId(), artifactMetaData.getArtifactPath());
                repositoryPath.setArtifact(artifact);
                artifactEvent.dispatchArtifactMetaDataEvent(repositoryPath);
            } catch (Exception e) {
                log.error("批量新增制品元数据错误：{}", ExceptionUtils.getStackTrace(e));
                throw new RuntimeException("批量新增制品元数据错误，请稍后重试");
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
        List<FileContent> fileContents = directoryListing.getFiles().stream().filter(file -> DockerArtifactCoordinates.include(file.getName())).collect(Collectors.toList());
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
    public void buildGraphIndex(String username, String storageId, String repositoryId, String path, Boolean metadata, Integer batch) {
        log.info("BuildGraphIndex is starting...");
        Long dictId = 0L;
        try {
            Dict existsDict = dictService.selectLatestOneDict(Dict.builder().dictType(DictTypeEnum.BUILD_GRAPH_INDEX.getType()).build());
            String comment = "构建中";
            if (Objects.nonNull(existsDict) && comment.equals(existsDict.getComment())) {
                return;
            }
            JSONObject data = new JSONObject();
            data.put("storageId", storageId);
            data.put("repositoryId", repositoryId);
            data.put("path", path);
            data.put("metadata", metadata);
            data.put("batch", batch);
            Dict dict = Dict.builder().dictType(DictTypeEnum.BUILD_GRAPH_INDEX.getType()).dictKey(username).dictValue(data.toJSONString()).createTime(new Date()).comment(comment).build();
            dictService.saveDict(dict);
            dictId = dict.getId();
            if (StringUtils.isNotBlank(storageId) && StringUtils.isNotBlank(repositoryId)) {
                handleRepository(storageId, repositoryId, path, metadata, batch);
            } else if (StringUtils.isNotBlank(storageId)) {
                path = "";
                Map<String, ? extends Repository> repositoryMaps = configurationManagementService.getMutableConfigurationClone().getStorage(storageId).getRepositories();
                if (!repositoryMaps.isEmpty()) {
                    for (String repository : repositoryMaps.keySet()) {
                        handleRepository(storageId, repository, path, metadata, batch);
                    }
                }
            } else if (StringUtils.isBlank(storageId) && StringUtils.isBlank(repositoryId)) {
                path = "";
                Map<String, StorageDto> storageMap = configurationManagementService.getMutableConfigurationClone().getStorages();
                if (!storageMap.isEmpty()) {
                    for (Map.Entry<String, StorageDto> storageEntry : storageMap.entrySet()) {
                        Map<String, ? extends Repository> repositoryMaps = configurationManagementService.getMutableConfigurationClone().getStorage(storageEntry.getKey()).getRepositories();
                        if (!repositoryMaps.isEmpty()) {
                            for (String repository : repositoryMaps.keySet()) {
                                handleRepository(storageEntry.getKey(), repository, path, metadata, batch);
                            }
                        }
                    }
                }
            }
            dictService.updateDict(DictForm.builder().id(dictId).comment("构建完成").build());
        } catch (Exception ex) {
            log.error("BuildGraphIndex is error [{}]", ExceptionUtils.getStackTrace(ex));
            dictService.updateDict(DictForm.builder().id(dictId).comment("构建错误").build());
        }
        log.info("BuildGraphIndex is finished");
    }

    @Override
    public StatusInfo store(String username, String storageId, String repositoryId, String path, String uuid, MultipartFile file) {
        String parentPath = tempPath + File.separator + UUID.fastUUID().toString();
        File parentFile = new File(parentPath);
        StatusInfo statusInfo = StatusInfo.builder().total(0).success(0).fail(0).build();
        try (InputStream inputStream = file.getInputStream()) {
            String fileOriginalName = ((CommonsMultipartFile) file).getFileItem().getName();
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
                        try (FileInputStream fileInputStream = new FileInputStream(artifactFile)) {
                            flag = storeArtifact(repositoryPath, fileInputStream);
                            if (flag) {
                                successTotal = successTotal + 1;
                            }
                        }
                    } catch (Exception ex) {
                        log.error("路径 [{}] 错误 [{}] ", artifactFile.getAbsolutePath(), ExceptionUtils.getStackTrace(ex));
                    }
                    statusInfo.setSuccess(successTotal);
                    statusInfo.setFail(statusInfo.getTotal() - statusInfo.getSuccess());
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
                    if (artifact.getArtifactCoordinates() instanceof MavenArtifactCoordinates) {
                        MavenArtifactCoordinates mavenArtifactCoordinates = (MavenArtifactCoordinates) artifact.getArtifactCoordinates();
                        artifactInfo.setName(String.format("%s:%s", mavenArtifactCoordinates.getGroupId(), mavenArtifactCoordinates.getArtifactId()));
                    }
                } else if (Objects.nonNull(repository) && DockerLayoutProvider.ALIAS.equals(repository.getLayout())) {
                    if (artifact.getArtifactCoordinates() instanceof DockerArtifactCoordinates) {
                        DockerArtifactCoordinates dockerArtifactCoordinates = (DockerArtifactCoordinates) artifact.getArtifactCoordinates();
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
                scanService.doScan(artifact);
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
        String filename = FilenameUtils.getName(repositoryPath.getFileName().toString());
        String filePath = "." + filename + ".foLibrary-metadata/bom.json";
        RepositoryPath bomRepositoryPath = repositoryPath.getParent().resolve(filePath);
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
                String bom = stringBuilder.toString();
                if (!JSONUtil.isJson(bom)) {
                    throw new IllegalArgumentException("BOM content must be in JSON format");
                }
                JSONObject bomJson = new JSONObject();
                bomJson.put("bomId", "");
                bomJson.put("bomValue", JSONObject.parseObject(bom));
                Files.write(bomRepositoryPath, bomJson.toJSONString().getBytes(StandardCharsets.UTF_8), StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.CREATE);
            }
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException(ex.getMessage());
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
                if (artifact.getArtifactCoordinates() instanceof DockerArtifactCoordinates) {
                    DockerArtifactCoordinates dockerArtifactCoordinates = (DockerArtifactCoordinates) artifact.getArtifactCoordinates();
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
                artifactMetadataService.rebuildMetadata(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), repositoryPath.getArtifactEntry().getArtifactPath());
            } catch (Exception ex) {
                log.error("StoreArtifact rebuildMetadata repositoryPath：{}，error：{}", repositoryPath.toString(), ExceptionUtils.getStackTrace(ex));
            }
        } catch (Exception ex) {
            log.error("StoreArtifact repositoryPath：{} error：{}", repositoryPath.toString(), ExceptionUtils.getStackTrace(ex));
            return false;
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
     */
    private void handleRepository(String storageId, String repositoryId, String path, Boolean metadata, Integer batch) {
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
            handleArtifacts(repositoryPath, repositoryPath.getRepository(), metadata, batch);
            log.info("StorageId [{}] repositoryId [{}] is finished", storageId, repositoryId);
        } catch (Exception ex) {
            log.error("StorageId [{}] repositoryId [{}] error [{}]", storageId, repositoryId, ExceptionUtils.getStackTrace(ex));
        }
    }

    /**
     * 向其他集群节点同步元数据配置
     *
     * @param mutableMetadataConfiguration 元数据
     * @param syncMetadataEnum             枚举类型
     */
    private void syncDataMetadataConfiguration(MutableMetadataConfiguration mutableMetadataConfiguration, SyncMetadataEnum syncMetadataEnum) {
        SyncMetadataDto syncMetadataDto = SyncMetadataDto.builder().syncMetadataEnum(syncMetadataEnum).
                mutableMetadataConfiguration(mutableMetadataConfiguration).build();
        clusterSyncService.syncMetadataConfiguration(syncMetadataDto);
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
            storageIdList = new ArrayList<>(configurationManagementService.getMutableConfigurationClone().getStorages().keySet());
            return storageIdList;
        }
        for (Map.Entry<String, StorageDto> entry : configurationManagementService.getMutableConfigurationClone().getStorages().entrySet()) {
            Set<String> userSet = entry.getValue().getUsers();
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
        Example example = new Example(ScanRules.class);
        example.createCriteria().andEqualTo("onScan", onScan).andIn("id", storageIdAndRepositoryIdList);
        List<ScanRules> scanRulesList = scanRulesMapper.selectByExample(example);
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
     * @return NFS目录下的所有文件
     */
    private List<RepositoryPath> handleArtifacts(RepositoryPath rootRepositoryPath, Repository repository, Boolean metadata, Integer batch) throws Exception {
        List<RepositoryPath> resultList = RepositoryPathUtil.getPaths(repository.getLayout(), rootRepositoryPath, Lists.newArrayList(DockerLayoutProvider.BLOBS, DockerLayoutProvider.MANIFEST));
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
                                        artifactManagementService.validateAndStoreIndex(blobPath);
                                    }
                                    if (StringUtils.isNotBlank(manifest.getDigest())) {
                                        RepositoryPath mainFestPath = repositoryPathResolver.resolve(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), DockerLayoutProvider.MANIFEST + File.separator + manifest.getDigest());
                                        artifactManagementService.validateAndStoreIndex(mainFestPath);
                                    }
                                }
                            }
                        }
                        if (Boolean.TRUE.equals(metadata)) {
                            handlerMetadata(artifactPath, repositoryPath);
                        }
                        artifactManagementService.validateAndStoreIndex(repositoryPath);
                        if (Maven2LayoutProvider.ALIAS.equals(repository.getLayout())) {
                            try {
                                artifactMetadataService.rebuildMetadata(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), repositoryPath.getArtifactEntry().getArtifactPath());
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
            List<Storage> storageList = new ArrayList<>(configurationManagementService.getMutableConfigurationClone().getStorages().values());
            for (Storage storage : storageList) {
                storageIdAndRepositoryIdList.addAll(storage.getRepositories().values().stream().map(item -> String.format("%s-%s", storage.getId(), item.getId())).collect(Collectors.toList()));
            }
            return storageIdAndRepositoryIdList;
        }
        List<Storage> storageList = configurationManagementService.getMutableConfigurationClone().getStorages().values().stream().filter(item -> storageIdList.contains(item.getId())).collect(Collectors.toList());
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

    private List<String> getGroupStorageIdAndRepositoryId(com.veadan.folib.storage.repository.Repository repository) {
        List<String> storageIdAndRepositoryIdList = Lists.newArrayList();
        for (String storageAndRepositoryId : repository.getGroupRepositories()) {
            String sId = ConfigurationUtils.getStorageId(repository.getStorage().getId(), storageAndRepositoryId);
            String rId = ConfigurationUtils.getRepositoryId(storageAndRepositoryId);
            com.veadan.folib.storage.repository.Repository subRepository = configurationManagementService.getConfiguration().getRepository(sId, rId);
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

    private void handlerMetadata(String artifactPath, RepositoryPath repositoryPath) {
        try {
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
                promotionUtil.setMetaData(repositoryPath, JSONObject.toJSONString(itemMetadataJson));
            }
            String fileName = "." + FilenameUtils.getName(repositoryPath.getFileName().toString()) + ".metadata";
            RepositoryPath artifactMetadataRepositoryPath = repositoryPath.getParent().resolve(fileName);
            if (Files.exists(artifactMetadataRepositoryPath)) {
                try (InputStream inputStream = Files.newInputStream(artifactMetadataRepositoryPath);
                     ObjectInputStream objectInputStream = new ObjectInputStream(inputStream)) {
                    Artifact artifact = (Artifact) objectInputStream.readObject();
                    if (Objects.nonNull(artifact) && StringUtils.isNotBlank(artifact.getMetadata())) {
                        promotionUtil.setMetaData(repositoryPath, artifact.getMetadata());
                    }
                } catch (Exception ex) {
                    Files.deleteIfExists(artifactMetadataRepositoryPath);
                    log.warn("解析制品 [{}] 本地缓存.metadata文件错误", ExceptionUtils.getStackTrace(ex));
                }
            }
        } catch (Exception ex) {
            log.error("handleArtifact sync metadata path：{}，error：{}", repositoryPath.toString(), ExceptionUtils.getStackTrace(ex));
        }
    }

    private Repository getRepository(String storageId, String repositoryId) {
        return configurationManagementService.getConfiguration().getRepository(storageId, repositoryId);
    }

}
