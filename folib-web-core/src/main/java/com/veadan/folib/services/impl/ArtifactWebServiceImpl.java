package com.veadan.folib.services.impl;

import cn.hutool.core.date.DatePattern;
import cn.hutool.core.date.DateUtil;
import com.alibaba.excel.EasyExcel;
import com.alibaba.excel.ExcelWriter;
import com.alibaba.excel.write.metadata.WriteSheet;
import com.alibaba.excel.write.metadata.fill.FillConfig;
import com.alibaba.fastjson.JSONObject;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.veadan.folib.cloud.storage.s3fs.S3Iterator;
import com.veadan.folib.cloud.storage.s3fs.S3Path;
import com.veadan.folib.cluster.SyncMetadataEnum;
import com.veadan.folib.configuration.MutableMetadataConfiguration;
import com.veadan.folib.controllers.ResponseMessage;
import com.veadan.folib.controllers.cluster.dto.SyncMetadataDto;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.ArtifactMetadata;
import com.veadan.folib.domain.DirectoryListing;
import com.veadan.folib.domain.FileContent;
import com.veadan.folib.forms.artifact.ArtifactMetadataForm;
import com.veadan.folib.forms.scanner.*;
import com.veadan.folib.gremlin.dsl.EntityTraversalUtils;
import com.veadan.folib.gremlin.entity.vo.ArtifactVo;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.layout.DockerLayoutProvider;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.scanner.entity.ScanRules;
import com.veadan.folib.scanner.mapper.ScanRulesMapper;
import com.veadan.folib.services.*;
import com.veadan.folib.storage.StorageDto;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.util.CustomDateUtils;
import com.veadan.folib.util.FileSizeConvertUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import tk.mybatis.mapper.entity.Example;

import javax.inject.Inject;
import javax.servlet.http.HttpServletResponse;
import javax.transaction.Transactional;
import java.io.IOException;
import java.io.InputStream;
import java.net.URLEncoder;
import java.nio.file.Path;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.*;
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

    @Override
    public void exportExcel(String vulnerabilityUuid, String storageId, String repositoryId, HttpServletResponse response) throws IOException {
        List<Artifact> artifactList = artifactRepository.findMatchingByVulnerabilityUuid(vulnerabilityUuid, storageId, repositoryId);
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
                                String path = artifact.getArtifactPath();
                                //docker
                                artifactVo.setName(path.substring(0, path.indexOf("/blobs/sha256")));
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
        } catch (Exception ex) {
            log.error("=====>>>>>保存制品元数据错误：{}", ExceptionUtils.getStackTrace(ex));
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
            if (Objects.nonNull(metadataJson) && metadataJson.containsKey(key)) {
                ArtifactMetadata artifactMetadata = ArtifactMetadata.builder().build();
                BeanUtils.copyProperties(artifactMetadataForm, artifactMetadata);
                metadataJson.put(key, artifactMetadata);
                artifact.setMetadata(metadataJson.toJSONString());
                artifactService.saveOrUpdateArtifact(artifact);
            }
        } catch (Exception ex) {
            log.error("=====>>>>>修改制品元数据错误：{}", ExceptionUtils.getStackTrace(ex));
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
            }
        } catch (Exception ex) {
            log.error("=====>>>>>删除制品元数据错误：{}", ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException("删除制品元数据错误，请稍后重试");
        }
    }

    @Override
    public CountForm getCount(String username) {
        Long zero = 0L;
        CountForm countForm = CountForm.builder().scanCount(zero).notScanCount(zero).scanSuccessCount(zero).scanFailCount(zero)
                .dependencyCount(zero).dependencyVulnerabilitiesCount(zero).vulnerabilitiesCount(zero).suppressedVulnerabilitiesCount(zero).build();
        List<String> storageIds = havePermissionStorageIdList(username);
        List<String> storageIdAndRepositoryIdList = getStorageIdsRepositoryIdsByOnScanAndStorageIds(1, storageIds);
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
    public List<DayCountForm> monthCount(String username) {
        List<String> storageIds = havePermissionStorageIdList(username);
        List<String> dayList = CustomDateUtils.getDaysBetween(30);
        Map<String, Long> map = null;
        List<DayCountForm> list = Lists.newArrayList();
        Long zero = 0L, dependencyCount, vulnerabilitiesCount;
        List<String> storageIdAndRepositoryIdList = getStorageIdsRepositoryIdsByOnScanAndStorageIds(1, storageIds);
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
    public WeekCountForm weekCount(String username) {
        List<String> storageIds = havePermissionStorageIdList(username);
        List<String> currentWeekList = CustomDateUtils.getDaysBetween(7);
        List<String> lastWeekList = CustomDateUtils.getDaysBetween(14);
        lastWeekList.removeAll(currentWeekList);
        Map<String, Long> map = null;
        WeekCountForm weekCountForm = WeekCountForm.builder().build();
        List<WeekDayCountForm> list = Lists.newArrayList();
        Long zero = 0L, vulnerabilitiesCount;
        List<String> storageIdAndRepositoryIdList = getStorageIdsRepositoryIdsByOnScanAndStorageIds(1, storageIds);
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
    public List<RepositoryCountForm> repositories(String username) {
        List<String> storageIds = havePermissionStorageIdList(username);
        Example example = new Example(ScanRules.class);
        example.createCriteria().andEqualTo("onScan", 1).andIn("storage", storageIds);
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
            if (repositoryCountForm.getScanCount() == 0) {
                r = "100";
            } else {
                r = decimalFormat.format((float) repositoryCountForm.getDependencyVulnerabilitiesCount() / (float) repositoryCountForm.getScanCount() * 100);
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
                repositoryForm.setFilePath(repositoryForm.getFilePaths().get(0).getFilePath());
            }
            return repositoryForm;
        }).collect(Collectors.toList()));
        return repositoryScannerForm;
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
        Path path = repositoryPath.getTarget();
        String artifactPath = "";
        if (path instanceof S3Path) {
            //S3存储
            S3Path s3Path = (S3Path) path;
            S3Iterator iterators = new S3Iterator(s3Path);
            S3Path imagePath = null;
            while (iterators.hasNext()) {
                S3Path itemS3Path = iterators.next();
                if (!itemS3Path.endsWith(".sha256")) {
                    imagePath = itemS3Path;
                    break;
                }
            }
            if (Objects.nonNull(imagePath)) {
                artifactPath = imagePath.getKey().replace(String.format("%s/%s/", repositoryPath.getStorageId(), repositoryPath.getRepositoryId()), "");
            }
        } else {
            DirectoryListing directoryListing = directoryListingService.fromRepositoryPath(repositoryPath);
            List<FileContent> fileContents = directoryListing.getFiles().stream().filter(file -> !(file.getName().endsWith(".sha256"))).collect(Collectors.toList());
            FileContent fileContent = fileContents.get(0);
            artifactPath = fileContent.getArtifactPath();
        }
        return artifactRepository.findOneArtifact(storageId, repositoryId, artifactPath);
    }

    /***
     * 获取制品RepositoryPath
     * @param storageId 存储空间名称
     * @param repositoryId 仓库名称
     * @param artifactPath 制品路径
     * @return RepositoryPath
     * @throws Exception 异常
     */
    private Artifact resolvePath(String storageId, String repositoryId, String artifactPath) throws Exception {
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
        Artifact artifact = Objects.nonNull(repositoryPath) ? repositoryPath.getArtifactEntry() : null;
        if (Objects.isNull(artifact)) {
            //兼容已存在数据的docker布局仓库
            Repository repository = configurationManagementService.getConfiguration().getRepository(storageId, repositoryId);
            if (DockerLayoutProvider.ALIAS.equalsIgnoreCase(repository.getLayout())) {
                //docker
                artifact = getDockerArtifact(artifactPath, storageId, repositoryId);
                return artifact;
            }
        }
        return artifact;
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

    /**
     * 获取有权限访问的存储空间id列表
     *
     * @return 有权限访问的存储空间id列表
     */
    public List<String> havePermissionStorageIdList(String username) {
        List<String> storageIdList = Lists.newArrayList();
        String admin = "admin";
        if (admin.equals(username)) {
            storageIdList = new ArrayList<>(configurationManagementService.getMutableConfigurationClone().getStorages().keySet());
            return storageIdList;
        }
        for (Map.Entry<String, StorageDto> entry : configurationManagementService.getMutableConfigurationClone().getStorages().entrySet()) {
            Set<String> userSet = entry.getValue().getUsers();
            if (CollectionUtils.isNotEmpty(userSet)) {
                if (userSet.contains(username)) {
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
        Example example = new Example(ScanRules.class);
        example.createCriteria().andEqualTo("onScan", onScan).andIn("storage", storageIds);
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
}
