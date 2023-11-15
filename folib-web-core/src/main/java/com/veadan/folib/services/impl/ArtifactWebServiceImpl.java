package com.veadan.folib.services.impl;

import cn.hutool.core.date.DatePattern;
import cn.hutool.core.date.DateUtil;
import cn.hutool.core.io.FileUtil;
import cn.hutool.core.io.IORuntimeException;
import cn.hutool.core.lang.UUID;
import cn.hutool.extra.spring.SpringUtil;
import com.alibaba.excel.EasyExcel;
import com.alibaba.excel.ExcelWriter;
import com.alibaba.excel.write.metadata.WriteSheet;
import com.alibaba.excel.write.metadata.fill.FillConfig;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.xml.XmlMapper;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.veadan.folib.artifact.coordinates.DockerArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.MavenArtifactCoordinates;
import com.veadan.folib.authorization.dto.Role;
import com.veadan.folib.cloud.storage.s3fs.S3FileSystem;
import com.veadan.folib.cloud.storage.s3fs.S3Iterator;
import com.veadan.folib.cloud.storage.s3fs.S3Path;
import com.veadan.folib.cluster.SyncMetadataEnum;
import com.veadan.folib.components.artifact.ArtifactComponent;
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
import com.veadan.folib.providers.layout.LayoutProviderRegistry;
import com.veadan.folib.providers.layout.Maven2LayoutProvider;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.scanner.common.msg.TableResultResponse;
import com.veadan.folib.scanner.entity.ScanRules;
import com.veadan.folib.scanner.mapper.ScanRulesMapper;
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
import java.net.URI;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.*;
import java.util.concurrent.FutureTask;
import java.util.stream.Collectors;

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
    private FqlSearchService fqlSearchService;

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
            // 这里URLEncoder.encode可以防止中文乱码
            String fileName = URLEncoder.encode(vulnerabilityUuid + "影响范围", "utf-8").replaceAll("\\+", "%20");
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
        String finalPrefix = prefix;
        repositoryScannerForm.setList(artifactPage.getContent().stream().map(artifact -> {
            String scanTime = DateUtil.format(Date.from(artifact.getScanDateTime().atZone(ZoneId.of("Asia/Shanghai")).toOffsetDateTime().toInstant()), df);
            RepositoryForm repositoryForm = RepositoryForm.builder().dependencyCount(artifact.getDependencyCount()).dependencyVulnerabilitiesCount(artifact.getDependencyVulnerabilitiesCount())
                    .uuid(artifact.getUuid()).scanTime(scanTime).suppressedVulnerabilitiesCount(artifact.getSuppressedVulnerabilitiesCount())
                    .vulnerabilitiesCount(artifact.getVulnerabilitiesCount()).storageId(artifact.getStorageId()).repositoryId(artifact.getRepositoryId()).artifactPath(artifact.getArtifactPath()).build();
            repositoryForm.setFilePaths(Optional.ofNullable(artifact.getFilePaths()).orElse(Collections.emptySet()).stream().map(item -> JSONObject.parseObject(item, ScannerReportForm.class)).collect(Collectors.toList()));
            if (DockerLayoutProvider.ALIAS.equals(repository.getLayout())) {
                //docker
                String uuid = artifact.getUuid();
                repositoryForm.setImageName(uuid.substring(finalPrefix.length(), uuid.indexOf("/")));
                repositoryForm.setVersion(uuid.substring(uuid.indexOf("/") + 1, uuid.indexOf("/sha256")));
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
        List<FileContent> fileContents = directoryListing.getFiles().stream().filter(file -> !(file.getName().endsWith(".sha256"))).collect(Collectors.toList());
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
                artifactPath = URLDecoder.decode(artifactPath, StandardCharsets.UTF_8);
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
        log.info("BuildGraphIndex is started");
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
                handlerRepository(storageId, repositoryId, path, metadata, batch);
            } else if (StringUtils.isNotBlank(storageId)) {
                path = "";
                Map<String, ? extends Repository> repositoryMaps = configurationManagementService.getMutableConfigurationClone().getStorage(storageId).getRepositories();
                if (!repositoryMaps.isEmpty()) {
                    for (String repository : repositoryMaps.keySet()) {
                        handlerRepository(storageId, repository, path, metadata, batch);
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
                                handlerRepository(storageEntry.getKey(), repository, path, metadata, batch);
                            }
                        }
                    }
                }
            }
            dictService.updateDict(DictForm.builder().id(dictId).comment("构建完成").build());
        } catch (Exception ex) {
            log.error("BuildGraphIndex is error：{}", ExceptionUtils.getStackTrace(ex));
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
    private void handlerRepository(String storageId, String repositoryId, String path, Boolean metadata, Integer batch) {
        try {
            log.info("handlerRepository storageId：{}，repositoryId：{} start", storageId, repositoryId);
            RootRepositoryPath rootRepositoryPath = repositoryPathResolver.resolve(storageId, repositoryId);
            if (StringUtils.isBlank(path)) {
                path = rootRepositoryPath.toAbsolutePath().toString();
            }
            handlerArtifacts(path, rootRepositoryPath.getRepository(), metadata, batch);
            log.info("handlerRepository storageId：{}，repositoryId：{} finished", storageId, repositoryId);
        } catch (Exception ex) {
            log.error("handlerRepository storageId：{}，repositoryId：{} error：{}", storageId, repositoryId, ExceptionUtils.getStackTrace(ex));
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

    private void handlerArtifacts(String path, Repository repository, Boolean metadata, Integer batch) throws Exception {
        if (Objects.isNull(batch)) {
            batch = 500;
        }
        String s3 = "s3://";
        if (path.startsWith(s3)) {
            S3Path s3Path = new S3Path(SpringUtil.getBean(S3FileSystem.class), path);
            handlerS3Paths(s3Path, repository, metadata, batch);
        } else {
            handlerNFSFiles(path, repository, metadata, batch);
        }
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
                    log.info("file：{} is a docker layout file", rootFile.getName());
                    return Collections.emptyList();
                }
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
                            log.info("file：{} is a docker layout file", f.getName());
                            continue;
                        }
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
                            log.info("directory：{} is a hidden directory", f.getName());
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
                            log.info("file：{} is a docker layout file", f.getName());
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
     * 处理NFS存储制品
     *
     * @param path       NFS目录
     * @param repository 仓库信息
     * @param metadata   是否同步元数据 true 是 false 否
     * @param batch      每批数量
     * @return NFS目录下的所有文件
     */
    private List<File> handlerNFSFiles(String path, Repository repository, Boolean metadata, Integer batch) throws Exception {
        List<File> resultList = getNFSFiles(path, repository);
        List<List<File>> fileLists = Lists.partition(resultList, batch);
        List<FutureTask<String>> futureTaskList = Lists.newArrayList();
        FutureTask<String> futureTask = null;
        for (List<File> fileList : fileLists) {
            futureTask = new FutureTask<String>(() -> {
                String fPath, tempStr, artifactPath;
                RepositoryPath repositoryPath;
                int fPathIndex;
                for (File file : fileList) {
                    fPath = file.getAbsolutePath();
                    try {
                        tempStr = repository.getStorage().getId() + File.separator + repository.getId() + File.separator;
                        fPathIndex = fPath.lastIndexOf(tempStr);
                        artifactPath = fPath.substring(fPathIndex).replace(tempStr, "");
                        repositoryPath = repositoryPathResolver.resolve(repository.getStorage().getId(), repository.getId(), artifactPath);
                        if (!RepositoryFiles.isArtifact(repositoryPath)) {
                            log.info("handlerArtifact path：{} not is a artifact", fPath);
                            continue;
                        }
                        if (Boolean.TRUE.equals(metadata)) {
                            handlerMetadata(artifactPath, repositoryPath);
                        }
                        artifactManagementService.validateAndStoreIndex(repositoryPath);
                        if (Maven2LayoutProvider.ALIAS.equals(repository.getLayout())) {
                            try {
                                artifactMetadataService.rebuildMetadata(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), repositoryPath.getArtifactEntry().getArtifactPath());
                            } catch (Exception ex) {
                                log.error("handlerArtifact rebuildMetadata path：{}，error：{}", repositoryPath.toString(), ExceptionUtils.getStackTrace(ex));
                            }
                        }
                    } catch (Exception ex) {
                        log.error("handlerArtifact path：{} error：{}", fPath, ExceptionUtils.getStackTrace(ex));
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
        log.info("handlerNFSFiles is finished");
        return resultList;
    }

    private List<S3Path> getS3Paths(S3Path s3Path, Repository repository) {
        List<S3Path> listFile = new ArrayList<>();
        List<S3Path> listDir = new ArrayList<>();
        boolean dockerLayout = DockerLayoutProvider.ALIAS.equalsIgnoreCase(repository.getLayout());
        S3Iterator s3Iterator = new S3Iterator(s3Path);
        if (!s3Iterator.hasNext()) {
            if (dockerLayout && !s3Path.getFileName().toString().contains("sha256")) {
                log.info("s3 file：{} is a docker layout file", s3Path);
                return listFile;
            }
            listFile.add(s3Path);
        }
        while (s3Iterator.hasNext()) {
            S3Path s3PathTemp = s3Iterator.next();
            if (s3PathTemp.getFileAttributes() == null || s3PathTemp.getFileAttributes().isDirectory()) {
                if (s3PathTemp.getFileName().toString().startsWith(".")) {
                    log.info("s3 directory {} is a hidden directory skip", s3PathTemp);
                    continue;
                }
                if (s3PathTemp.getFileName().toString().endsWith(".artifactory-metadata")) {
                    log.info("s3 directory {} is a artifactory metadata directory skip", s3PathTemp);
                    continue;
                }
                listDir.add(s3PathTemp);
            } else {
                if (s3PathTemp.getFileName().toString().startsWith(".")) {
                    log.info("s3 file {} is a hidden file", s3PathTemp);
                    continue;
                }
                if (dockerLayout && !s3PathTemp.getFileName().toString().contains("sha256")) {
                    log.info("s3 file：{} is a docker layout file", s3PathTemp);
                    continue;
                }
                log.info("s3 file {}", s3PathTemp);
                listFile.add(s3PathTemp);
            }
        }
        while (listDir.size() != 0) {
            S3Path currentPath = listDir.get(0);
            listDir.remove(currentPath);
            s3Iterator = new S3Iterator(currentPath);
            while (s3Iterator.hasNext()) {
                S3Path s3PathTemp = s3Iterator.next();
                if (s3PathTemp.getFileAttributes() == null || s3PathTemp.getFileAttributes().isDirectory()) {
                    if (s3PathTemp.getFileName().toString().startsWith(".")) {
                        log.info("s3 directory {} is a hidden directory", s3PathTemp);
                        continue;
                    }
                    listDir.add(s3PathTemp);
                } else {
                    if (s3PathTemp.getFileName().toString().startsWith(".")) {
                        log.info("s3 file {} is a hidden file", s3PathTemp);
                        continue;
                    }
                    if (dockerLayout && !s3PathTemp.getFileName().toString().contains("sha256")) {
                        log.info("s3 file：{} is a docker layout file", s3PathTemp);
                        continue;
                    }
                    log.info("s3 file {}", s3PathTemp);
                    listFile.add(s3PathTemp);
                }
            }
        }
        log.info("s3Path [{}]  file size：{}", s3Path.toUri().toString(), listFile.size());
        return listFile;
    }


    /**
     * 处理S3存储制品
     *
     * @param s3Path     S3目录
     * @param repository 仓库信息
     * @param metadata   是否同步元数据 true 是 false 否
     * @param batch      每批数量
     * @return S3存储目录下的所有文件
     */
    private List<S3Path> handlerS3Paths(S3Path s3Path, Repository repository, Boolean metadata, Integer batch) throws Exception {
        List<S3Path> listFile = getS3Paths(s3Path, repository);
        List<List<S3Path>> s3PathLists = Lists.partition(listFile, batch);
        List<FutureTask<String>> futureTaskList = Lists.newArrayList();
        FutureTask<String> futureTask = null;
        for (List<S3Path> s3PathList : s3PathLists) {
            futureTask = new FutureTask<String>(() -> {
                for (S3Path s3FilePath : s3PathList) {
                    try {
                        String fPath = s3FilePath.toString();
                        String tempStr = repository.getStorage().getId() + File.separator + repository.getId() + File.separator;
                        int fPathIndex = fPath.lastIndexOf(tempStr);
                        String artifactPath = fPath.substring(fPathIndex).replace(tempStr, "");
                        RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository.getStorage().getId(), repository.getId(), artifactPath);
                        if (!RepositoryFiles.isArtifact(repositoryPath)) {
                            log.info("handlerArtifact path：{} not is a artifact", s3FilePath.toAbsolutePath());
                            continue;
                        }
                        if (Boolean.TRUE.equals(metadata)) {
                            handlerMetadata(artifactPath, repositoryPath);
                        }
                        artifactManagementService.validateAndStoreIndex(repositoryPath);
                        if (Maven2LayoutProvider.ALIAS.equals(repository.getLayout())) {
                            try {
                                artifactMetadataService.rebuildMetadata(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), repositoryPath.getArtifactEntry().getArtifactPath());
                            } catch (Exception ex) {
                                log.error("handlerArtifact rebuildMetadata path：{}，error：{}", repositoryPath.toString(), ExceptionUtils.getStackTrace(ex));
                            }
                        }
                    } catch (Exception ex) {
                        log.error("handlerArtifact path：{} error：{}", s3FilePath.toAbsolutePath(), ExceptionUtils.getStackTrace(ex));
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
        log.info("handlerS3Paths is finished");
        return listFile;
    }

    private String s3Manifest(List<S3Path> s3FilesPaths, StringBuilder manifestBuilder) throws IOException {
        if (CollectionUtils.isEmpty(s3FilesPaths)) {
            return "";
        }
        List<S3Path> fileContents = s3FilesPaths.stream().filter(file -> !(file.toAbsolutePath().endsWith(".sha256"))).collect(Collectors.toList());
        S3Path filePath = fileContents.get(0);
        String[] array = filePath.getKey().split(File.separator);
        manifestBuilder.append(array[array.length - 1]);
        return Files.readString(filePath);
    }

    private String nfsManifest(List<File> fileList, StringBuilder manifestBuilder) throws IOException {
        if (CollectionUtils.isEmpty(fileList)) {
            return "";
        }
        List<File> fileContents = fileList.stream().filter(file -> !(file.getName().endsWith(".sha256"))).collect(Collectors.toList());
        File file = fileContents.get(0);
        manifestBuilder.append(file.getName());
        return Files.readString(file.toPath());
    }

    private void handlerDockerBlobAndManifest(String path, Repository repository, List<File> fileList, List<S3Path> s3FilesPaths) throws Exception {
        //判断是否是docker布局
        boolean dockerLayout = "docker".equalsIgnoreCase(repository.getLayout());
        String s3 = "s3://";
        if (dockerLayout) {
            // blobs manifest
            String tempStr = repository.getStorage().getId() + File.separator + repository.getId() + File.separator;
            int fPathIndex = path.lastIndexOf(tempStr);
            String relativizePath = path.substring(fPathIndex).replace(tempStr, "");
            String[] arrayPath = relativizePath.split(File.separator);
            if (arrayPath.length != 2) {
                return;
            }
            String manifestContent = "";
            StringBuilder manifestBuilder = new StringBuilder();
            if (path.startsWith(s3)) {
                manifestContent = s3Manifest(s3FilesPaths, manifestBuilder);
            } else {
                manifestContent = nfsManifest(fileList, manifestBuilder);
            }
            JSONObject manifest = JSON.parseObject(manifestContent);
            JSONArray layers = manifest.getJSONArray("layers");
            List<String> layerList = new ArrayList<>();
            for (int i = 0; i < layers.size(); i++) {
                layerList.add(layers.getJSONObject(i).getString("digest"));
            }
            String manifestConfig = manifest.getJSONObject("config").getString("digest");
            // blobs
            layerList.add(manifestConfig);
            for (String layer : layerList) {
                String blob = arrayPath[0] + File.separator + "blobs" + File.separator + layer;
                RepositoryPath blobPath = repositoryPathResolver.resolve(repository.getStorage().getId(), repository.getId(), blob);
                artifactManagementService.validateAndStoreIndex(blobPath);
            }
            // manifest
            String manifestFile = arrayPath[0] + File.separator + "manifest" + File.separator + manifestBuilder;
            RepositoryPath manifestPath = repositoryPathResolver.resolve(repository.getStorage().getId(), repository.getId(), manifestFile);
            artifactManagementService.validateAndStoreIndex(manifestPath);
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
                List<JSONObject> artifactMetadataList = Lists.newArrayList();
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
                    log.warn("解析制品 [{}] 本地缓存.metadata文件错误", ExceptionUtils.getStackTrace(ex));
                }
            }
        } catch (Exception ex) {
            log.error("handlerArtifact sync metadata path：{}，error：{}", repositoryPath.toString(), ExceptionUtils.getStackTrace(ex));
        }
    }

    private Repository getRepository(String storageId, String repositoryId) {
        return configurationManagementService.getConfiguration().getRepository(storageId, repositoryId);
    }

}
