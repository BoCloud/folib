package com.veadan.folib.services.impl;

import cn.hutool.core.date.DateUtil;
import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson.JSONObject;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.github.pagehelper.Page;
import com.github.pagehelper.PageHelper;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.collect.Sets;
import com.veadan.folib.components.DistributedCacheComponent;
import com.veadan.folib.components.IdGenerateUtils;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.configuration.ConfigurationUtils;
import com.veadan.folib.data.criteria.Paginator;
import com.veadan.folib.domain.StorageDevice;
import com.veadan.folib.entity.StorageMonitoring;
import com.veadan.folib.enums.DirectoryDataTypeEnum;
import com.veadan.folib.enums.StorageDeviceTypeEnum;
import com.veadan.folib.mapper.StorageMonitoringMapper;
import com.veadan.folib.model.request.StorageMonitoringReq;
import com.veadan.folib.model.response.StorageMonitoringRes;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.io.RootRepositoryPath;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.scanner.common.msg.TableResultResponse;
import com.veadan.folib.services.StorageMonitoringService;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import com.veadan.folib.utils.directory.DirectorySizeCalculatorUtils;
import com.veadan.folib.utils.directory.Result;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.inject.Inject;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.file.FileStore;
import java.nio.file.Files;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

/**
 * @author veadan
 * @date 2024/7/18
 **/
@Slf4j
@Service
public class StorageMonitoringServiceImpl implements StorageMonitoringService {

    @Autowired
    private StorageMonitoringMapper storageMonitoringMapper;

    @Autowired
    private ConfigurationManager configurationManager;

    @Autowired
    private RepositoryPathResolver repositoryPathResolver;

    @Autowired
    private ArtifactRepository artifactRepository;

    @Autowired
    private DistributedCacheComponent distributedCacheComponent;

    @Inject
    @Lazy
    private IdGenerateUtils idGenerateUtils;

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateStorageMonitoringData() {
        log.info("Update storage monitoring data start [{}]", DateUtil.now());
        long start = System.currentTimeMillis();
        Map<String, Storage> storageMap = getStorageMap();
        List<StorageMonitoring> storageMonitoringList = Lists.newArrayList();
        List<StorageDevice> storageDeviceList = Lists.newArrayList();
        Map<String, StorageDevice> storageDeviceMap = Maps.newConcurrentMap();
        handlerStorage(storageMap, storageMonitoringList, storageDeviceList, storageDeviceMap);
        Date date = new Date();
        storageStorageMonitoring(storageMonitoringList, date);
        platformStorageMonitoring(storageMap, storageMonitoringList, date);
        storageDeviceStorageMonitoring(storageMonitoringList, storageDeviceList, date);

        LocalDateTime deleteDeadlineDate = LocalDateTime.now().minusDays(60);
        storageMonitoringMapper.delete(Wrappers.<StorageMonitoring>lambdaQuery().le(StorageMonitoring::getCreateTime, deleteDeadlineDate));

        storageMonitoringMapper.update(StorageMonitoring.builder().isLatest(Boolean.FALSE).build(), Wrappers.<StorageMonitoring>lambdaUpdate().eq(StorageMonitoring::getIsLatest,true));
        List<List<StorageMonitoring>> lists = Lists.partition(storageMonitoringList, 50);
        for (List<StorageMonitoring> itemList : lists) {
            storageMonitoringMapper.batchInsertStorageMonitoring(itemList);
        }
        log.info("Update storage monitoring data end [{}] take time [{}] ms", DateUtil.now(), System.currentTimeMillis() - start);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void saveStorageMonitoring(StorageMonitoring storageMonitoring) {
        StorageMonitoringRes storageMonitoringRes = queryOneStorageMonitoring(storageMonitoring);
        if (Objects.nonNull(storageMonitoringRes)) {
            updateStorageMonitoring(storageMonitoring);
        } else {
            storageMonitoringMapper.insertOrUpdate(storageMonitoring);
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateStorageMonitoring(StorageMonitoring storageMonitoring) {
        StorageMonitoringRes storageMonitoringRes = queryOneStorageMonitoring(storageMonitoring);
        if (Objects.nonNull(storageMonitoringRes)) {
            storageMonitoringMapper.updateById(storageMonitoring);
        }
    }

    @Override
    public List<StorageMonitoringRes> queryStorageMonitoring(StorageMonitoring storageMonitoring) {
        List<StorageMonitoringRes> storageMonitoringResList = null;
        List<StorageMonitoring> storageMonitoringList = storageMonitoringMapper.selectList(Wrappers.<StorageMonitoring>lambdaQuery()
                .eq(StringUtils.isNotBlank(storageMonitoring.getStorageId()), StorageMonitoring::getStorageId, storageMonitoring.getStorageId())
                .eq(StringUtils.isNotBlank(storageMonitoring.getRepositoryId()), StorageMonitoring::getRepositoryId, storageMonitoring.getRepositoryId())
                .eq(StringUtils.isNotBlank(storageMonitoring.getRepositoryType()), StorageMonitoring::getRepositoryType, storageMonitoring.getRepositoryType())
                .eq(StringUtils.isNotBlank(storageMonitoring.getRepositoryLayout()), StorageMonitoring::getRepositoryLayout, storageMonitoring.getRepositoryLayout())
                .eq(Objects.nonNull(storageMonitoring.getDataType()), StorageMonitoring::getDataType, storageMonitoring.getDataType())
                .eq(Objects.nonNull(storageMonitoring.getIsLatest()), StorageMonitoring::getIsLatest, storageMonitoring.getIsLatest())
        );
        if (CollectionUtils.isNotEmpty(storageMonitoringList)) {
            storageMonitoringResList = storageMonitoringList.stream().map(item -> {
                StorageMonitoringRes storageMonitoringRes = StorageMonitoringRes.builder().build();
                BeanUtils.copyProperties(item, storageMonitoringRes);
                storageMonitoringRes.setCreateDate(DateUtil.formatDate(storageMonitoringRes.getCreateTime()));
                storageMonitoringRes.setCreateDay(DateUtil.format(storageMonitoringRes.getCreateTime(), "MM-dd"));
                return storageMonitoringRes;
            }).collect(Collectors.toList());
        }
        return storageMonitoringResList;
    }

    @Override
    public StorageMonitoringRes queryOneStorageMonitoring(StorageMonitoring storageMonitoring) {
        StorageMonitoringRes storageMonitoringRes = null;
        StorageMonitoring dbStorageMonitoring = storageMonitoringMapper.selectById(storageMonitoring.getId());
        if (Objects.nonNull(dbStorageMonitoring)) {
            storageMonitoringRes = StorageMonitoringRes.builder().build();
            BeanUtils.copyProperties(dbStorageMonitoring, storageMonitoringRes);
            storageMonitoringRes.setCreateDate(DateUtil.formatDate(storageMonitoringRes.getCreateTime()));
            storageMonitoringRes.setCreateDay(DateUtil.format(storageMonitoringRes.getCreateTime(), "MM-dd"));
        }
        return storageMonitoringRes;
    }

    @Override
    public TableResultResponse<StorageMonitoringRes> queryStorageMonitoringPage(StorageMonitoringReq storageMonitoringReq) {
        Integer page = storageMonitoringReq.getPage(), limit = storageMonitoringReq.getLimit();
        if (Objects.isNull(page)) {
            page = 1;
        }
        if (Objects.isNull(limit)) {
            limit = 10;
        }

        boolean isAsc = true;
        if (StringUtils.isNotBlank(storageMonitoringReq.getSortField()) && StringUtils.isNotBlank(storageMonitoringReq.getSortOrder())) {
            if (Paginator.Order.ASC.toString().equalsIgnoreCase(storageMonitoringReq.getSortOrder())) {
                isAsc =true;
            } else if (Paginator.Order.DESC.toString().equalsIgnoreCase(storageMonitoringReq.getSortOrder())) {
                isAsc = false;
            }
        }
        Page<Object> result = PageHelper.startPage(page, limit);
        List<StorageMonitoring> storageMonitoringList = storageMonitoringMapper.selectList(Wrappers.<StorageMonitoring>lambdaQuery()
                .eq(StringUtils.isNotBlank(storageMonitoringReq.getStorageId()), StorageMonitoring::getStorageId, storageMonitoringReq.getStorageId())
                .eq(StringUtils.isNotBlank(storageMonitoringReq.getRepositoryId()), StorageMonitoring::getRepositoryId, storageMonitoringReq.getRepositoryId())
                .eq(StringUtils.isNotBlank(storageMonitoringReq.getRepositoryType()), StorageMonitoring::getRepositoryType, storageMonitoringReq.getRepositoryType())
                .eq(StringUtils.isNotBlank(storageMonitoringReq.getRepositoryLayout()), StorageMonitoring::getRepositoryLayout, storageMonitoringReq.getRepositoryLayout())
                .eq(StringUtils.isNotBlank(storageMonitoringReq.getRepositorySubLayout()), StorageMonitoring::getRepositorySubLayout, storageMonitoringReq.getRepositorySubLayout())
                .eq(Objects.nonNull(storageMonitoringReq.getDataType()), StorageMonitoring::getDataType, storageMonitoringReq.getDataType())
                .in(CollectionUtils.isNotEmpty(storageMonitoringReq.getDataTypes()), StorageMonitoring::getDataType, storageMonitoringReq.getDataTypes())
                .eq(Objects.nonNull(storageMonitoringReq.getIsLatest()), StorageMonitoring::getIsLatest, storageMonitoringReq.getIsLatest())
                .eq(StringUtils.isNotBlank(storageMonitoringReq.getStorageDeviceName()), StorageMonitoring::getStorageDeviceName, storageMonitoringReq.getStorageDeviceName())
                .orderBy(true,isAsc,StorageMonitoring::getDataType)
        );
        List<StorageMonitoringRes> storageMonitoringResList = null;
        if (CollectionUtils.isNotEmpty(storageMonitoringList)) {
            storageMonitoringResList = storageMonitoringList.stream().map(item -> {
                StorageMonitoringRes storageMonitoringRes = StorageMonitoringRes.builder().build();
                BeanUtils.copyProperties(item, storageMonitoringRes);
                storageMonitoringRes.setCreateDate(DateUtil.formatDate(storageMonitoringRes.getCreateTime()));
                storageMonitoringRes.setCreateDay(DateUtil.format(storageMonitoringRes.getCreateTime(), "MM-dd"));
                return storageMonitoringRes;
            }).collect(Collectors.toList());
        }
        return new TableResultResponse<StorageMonitoringRes>(result.getTotal(), storageMonitoringResList);
    }

    private Map<String, Storage> getStorageMap() {
        return configurationManager.getConfiguration().getStorages();
    }

    private void handlerStorage(Map<String, Storage> storageMap, List<StorageMonitoring> storageMonitoringList, List<StorageDevice> storageDeviceList, Map<String, StorageDevice> storageDeviceMap) {
        Storage storage;
        Repository repository;
        RootRepositoryPath rootRepositoryPath;
        FileStore fileStore;
        StorageDevice storageDevice;
        StorageMonitoring storageMonitoring;
        long start;
        List<String> includeRepositories = getIncludeRepositories();
        List<String> excludeRepositories = getExcludeRepositories();
        String storageIdAndRepositoryId;
        for (String storageId : storageMap.keySet()) {
            storage = storageMap.get(storageId);
            Map<String, ? extends Repository> repositoryMap = storage.getRepositories();
            for (String repositoryId : repositoryMap.keySet()) {
                try {
                    repository = repositoryMap.get(repositoryId);
                    if (RepositoryTypeEnum.GROUP.getType().equals(repository.getType())) {
                        continue;
                    }
                    rootRepositoryPath = repositoryPathResolver.resolve(repository);
                    if (!Files.exists(rootRepositoryPath)) {
                        continue;
                    }
                    storageIdAndRepositoryId = ConfigurationUtils.getStorageIdAndRepositoryId(storageId, repositoryId);
                    if (CollectionUtils.isNotEmpty(includeRepositories) && !includeRepositories.contains(storageIdAndRepositoryId)) {
                        continue;
                    }
                    if (CollectionUtils.isNotEmpty(excludeRepositories) && excludeRepositories.contains(storageIdAndRepositoryId)) {
                        continue;
                    }
                    fileStore = Files.getFileStore(rootRepositoryPath);
                    long usedSpace = fileStore.getTotalSpace() - fileStore.getUsableSpace();
                    storageDevice = StorageDevice.builder().name(fileStore.name()).totalSpace(fileStore.getTotalSpace()).usableSpace(fileStore.getUsableSpace()).usedSpace(usedSpace).build();
                    storageDevice.setType(StorageDeviceTypeEnum.NAS.getType());
                    if (!storageDeviceMap.containsKey(storageId)) {
                        storageDeviceMap.put(storageId, storageDevice);
                    }
                    if (!storageDeviceList.contains(storageDevice)) {
                        storageDeviceList.add(storageDevice);
                    }
                    start = System.currentTimeMillis();
                    calculatorRepository(storageMonitoringList, storageDeviceMap, storage, repository, rootRepositoryPath);
                    log.info("Calculator repository [{}] [{}] data end [{}] take time [{}] ms", storage.getId(), repository.getId(), DateUtil.now(), System.currentTimeMillis() - start);
                } catch (Exception ex) {
                    log.error("Calculator storage monitoring data storageId [{}] repositoryId [{}] error [{}]", storageId, repositoryId, ExceptionUtils.getStackTrace(ex));
                }
            }
        }
    }

    private void storageDeviceStorageMonitoring(List<StorageMonitoring> storageMonitoringList, List<StorageDevice> storageDeviceList, Date date) {
        List<StorageMonitoring> repositoryStorageMonitoringList;
        StorageMonitoring storageDeviceStorageMonitoring;
        BigDecimal storageDeviceArtifactsSize = BigDecimal.ZERO, storageDeviceFilesSize = BigDecimal.ZERO, otherFilesSize;
        long storageDeviceArtifactsCount = 0L, storageDeviceFilesCount = 0L, storageDeviceFoldersCount = 0L, storageDeviceItemsCount = 0L, storageDeviceArtifactsDownloadedCount = 0;
        Set<String> storageSet = Sets.newHashSet();
        for (StorageDevice itemStorageDevice : storageDeviceList) {
            storageDeviceArtifactsSize = BigDecimal.ZERO;
            storageDeviceFilesSize = BigDecimal.ZERO;
            otherFilesSize = BigDecimal.ZERO;
            storageDeviceArtifactsCount = 0L;
            storageDeviceFilesCount = 0L;
            storageDeviceFoldersCount = 0L;
            storageDeviceItemsCount = 0L;
            storageSet = Sets.newHashSet();
            //存储设备级别相关数据统计，包含回收站数据
            repositoryStorageMonitoringList = storageMonitoringList.stream().filter(item -> (DirectoryDataTypeEnum.REPOSITORY.getType().equals(item.getDataType())) && itemStorageDevice.getName().equals(item.getStorageDeviceName())).collect(Collectors.toList());

            for (StorageMonitoring itemStorageDeviceStorageMonitoring : repositoryStorageMonitoringList) {
                storageSet.add(itemStorageDeviceStorageMonitoring.getStorageId());
                storageDeviceArtifactsCount = storageDeviceArtifactsCount + itemStorageDeviceStorageMonitoring.getArtifactsCount();
                storageDeviceArtifactsSize = storageDeviceArtifactsSize.add(itemStorageDeviceStorageMonitoring.getArtifactsSize());
                storageDeviceFilesCount = storageDeviceFilesCount + itemStorageDeviceStorageMonitoring.getFilesCount();
                storageDeviceFilesSize = storageDeviceFilesSize.add(itemStorageDeviceStorageMonitoring.getFilesSize());
                storageDeviceFoldersCount = storageDeviceFoldersCount + itemStorageDeviceStorageMonitoring.getFoldersCount();
                storageDeviceItemsCount = storageDeviceItemsCount + itemStorageDeviceStorageMonitoring.getItemsCount();
                if (Objects.nonNull(itemStorageDeviceStorageMonitoring.getArtifactsDownloadedCount())) {
                    storageDeviceArtifactsDownloadedCount = storageDeviceArtifactsDownloadedCount + itemStorageDeviceStorageMonitoring.getArtifactsDownloadedCount();
                }
            }
            storageDeviceStorageMonitoring = StorageMonitoring.builder().id(idGenerateUtils.generateId("storageMonitoringId")).dataType(DirectoryDataTypeEnum.STORAGE_DEVICE.getType()).isLatest(Boolean.TRUE).updateTime(date).build();
            storageDeviceStorageMonitoring.setArtifactsCount(storageDeviceArtifactsCount);
            storageDeviceStorageMonitoring.setArtifactsSize(storageDeviceArtifactsSize);
            storageDeviceStorageMonitoring.setFilesCount(storageDeviceFilesCount);
            storageDeviceStorageMonitoring.setFilesSize(storageDeviceFilesSize);
            storageDeviceStorageMonitoring.setFoldersCount(storageDeviceFoldersCount);
            storageDeviceStorageMonitoring.setItemsCount(storageDeviceItemsCount);
            storageDeviceStorageMonitoring.setArtifactsDownloadedCount(storageDeviceArtifactsDownloadedCount);
            storageDeviceStorageMonitoring.setStorageCount(storageSet.size());
            storageDeviceStorageMonitoring.setRepositoryCount((int) repositoryStorageMonitoringList.stream().filter(item -> DirectoryDataTypeEnum.REPOSITORY.getType().equals(item.getDataType())).count());
            storageDeviceStorageMonitoring.setStorageDeviceName(itemStorageDevice.getName());
            storageDeviceStorageMonitoring.setStorageDeviceType(itemStorageDevice.getType());
            storageDeviceStorageMonitoring.setStorageDeviceSize(BigDecimal.valueOf(itemStorageDevice.getTotalSpace()));
            storageDeviceStorageMonitoring.setStorageDeviceUsableSize(BigDecimal.valueOf(itemStorageDevice.getUsableSpace()));
            if (StorageDeviceTypeEnum.S3.getType().equalsIgnoreCase(itemStorageDevice.getType())) {
                storageDeviceStorageMonitoring.setUsedStorageDeviceSize(BigDecimal.valueOf(1099511627776000000L));
            } else {
                storageDeviceStorageMonitoring.setUsedStorageDeviceSize(BigDecimal.valueOf(itemStorageDevice.getUsedSpace()));
            }
            storageDeviceStorageMonitoring.setUsedStorageDeviceSizePercentage(storageDeviceStorageMonitoring.getUsedStorageDeviceSize().divide(storageDeviceStorageMonitoring.getStorageDeviceSize(), 4, RoundingMode.HALF_UP).multiply(BigDecimal.valueOf(100)));
            if (storageDeviceStorageMonitoring.getUsedStorageDeviceSize().compareTo(BigDecimal.ZERO) > 0) {
                otherFilesSize = storageDeviceStorageMonitoring.getUsedStorageDeviceSize().subtract(storageDeviceFilesSize);
            }
            storageDeviceStorageMonitoring.setUsedFilesSizePercentage(BigDecimal.ZERO);
            if (storageDeviceStorageMonitoring.getUsedStorageDeviceSize().compareTo(BigDecimal.ZERO) > 0) {
                storageDeviceStorageMonitoring.setUsedFilesSizePercentage(storageDeviceStorageMonitoring.getFilesSize().divide(storageDeviceStorageMonitoring.getUsedStorageDeviceSize(), 4, RoundingMode.HALF_UP).multiply(BigDecimal.valueOf(100)));
            }
            storageDeviceStorageMonitoring.setOtherFilesSize(otherFilesSize);
            storageDeviceStorageMonitoring.setUsedOtherFilesSizePercentage(BigDecimal.ZERO);
            if (storageDeviceStorageMonitoring.getUsedStorageDeviceSize().compareTo(BigDecimal.ZERO) > 0) {
                storageDeviceStorageMonitoring.setUsedOtherFilesSizePercentage(BigDecimal.valueOf(100).subtract(storageDeviceStorageMonitoring.getUsedFilesSizePercentage()));
            }
            storageMonitoringList.add(storageDeviceStorageMonitoring);
        }
    }

    private void platformStorageMonitoring(Map<String, Storage> storageMap, List<StorageMonitoring> storageMonitoringList, Date date) {
        BigDecimal platformArtifactsSize = BigDecimal.ZERO, platformFilesSize = BigDecimal.ZERO;
        long platformArtifactsCount = 0L, platformFilesCount = 0L, platformFoldersCount = 0L, platformItemsCount = 0L, platformArtifactsDownloadedCount = 0;
        StorageMonitoring platformStorageMonitoring = StorageMonitoring.builder().id(idGenerateUtils.generateId("storageMonitoringId")).dataType(DirectoryDataTypeEnum.PLATFORM.getType()).isLatest(Boolean.TRUE).updateTime(date).build();
        Set<String> platformStorageSet = Sets.newHashSet();
        Integer platformRepositoryCount = 0;
        for (StorageMonitoring itemStorageMonitoring : storageMonitoringList) {
            //平台级别相关数据统计，包含回收站数据
            if (!DirectoryDataTypeEnum.REPOSITORY.getType().equals(itemStorageMonitoring.getDataType())) {
                continue;
            }
            platformStorageSet.add(itemStorageMonitoring.getStorageId());
            if (DirectoryDataTypeEnum.REPOSITORY.getType().equals(itemStorageMonitoring.getDataType())) {
                platformRepositoryCount = platformRepositoryCount + 1;
                platformArtifactsDownloadedCount = platformArtifactsDownloadedCount + itemStorageMonitoring.getArtifactsDownloadedCount();
            }
            platformArtifactsCount = platformArtifactsCount + itemStorageMonitoring.getArtifactsCount();
            platformArtifactsSize = platformArtifactsSize.add(itemStorageMonitoring.getArtifactsSize());
            platformFilesCount = platformFilesCount + itemStorageMonitoring.getFilesCount();
            platformFilesSize = platformFilesSize.add(itemStorageMonitoring.getFilesSize());
            platformFoldersCount = platformFoldersCount + itemStorageMonitoring.getFoldersCount();
            platformItemsCount = platformItemsCount + itemStorageMonitoring.getItemsCount();
        }
        platformStorageMonitoring.setStorageCount(storageMap.size());
        platformStorageMonitoring.setRepositoryCount(platformRepositoryCount);
        platformStorageMonitoring.setArtifactsCount(platformArtifactsCount);
        platformStorageMonitoring.setArtifactsSize(platformArtifactsSize);
        platformStorageMonitoring.setFilesCount(platformFilesCount);
        platformStorageMonitoring.setFilesSize(platformFilesSize);
        platformStorageMonitoring.setFoldersCount(platformFoldersCount);
        platformStorageMonitoring.setItemsCount(platformItemsCount);
        platformStorageMonitoring.setArtifactsDownloadedCount(platformArtifactsDownloadedCount);
        storageMonitoringList.add(platformStorageMonitoring);
    }

    private void storageStorageMonitoring(List<StorageMonitoring> storageMonitoringList, Date date) {
        Map<String, List<StorageMonitoring>> storageIdMap = storageMonitoringList.stream().collect(Collectors.groupingBy(StorageMonitoring::getStorageId));
        List<StorageMonitoring> storageEntryStorageMonitoringList;
        BigDecimal storageArtifactsSize = BigDecimal.ZERO, storageFilesSize = BigDecimal.ZERO;
        long storageArtifactsCount = 0L, storageFilesCount = 0L, storageFoldersCount = 0L, storageItemsCount = 0L, storageArtifactsDownloadedCount;
        StorageMonitoring storageStorageMonitoring;
        for (Map.Entry<String, List<StorageMonitoring>> storageEntry : storageIdMap.entrySet()) {
            storageArtifactsSize = BigDecimal.ZERO;
            storageFilesSize = BigDecimal.ZERO;
            storageArtifactsCount = 0L;
            storageFilesCount = 0L;
            storageFoldersCount = 0L;
            storageItemsCount = 0L;
            storageArtifactsDownloadedCount = 0L;
            //存储空间级别相关数据统计，包含回收站数据
            storageStorageMonitoring = StorageMonitoring.builder().id(idGenerateUtils.generateId("storageMonitoringId")).dataType(DirectoryDataTypeEnum.STORAGE.getType()).storageId(storageEntry.getKey()).isLatest(Boolean.TRUE).updateTime(date).build();
            storageEntryStorageMonitoringList = storageEntry.getValue();
            for (StorageMonitoring storageStorageMonitoringItem : storageEntryStorageMonitoringList) {
                storageArtifactsCount = storageArtifactsCount + storageStorageMonitoringItem.getArtifactsCount();
                storageArtifactsSize = storageArtifactsSize.add(storageStorageMonitoringItem.getArtifactsSize());
                storageFilesCount = storageFilesCount + storageStorageMonitoringItem.getFilesCount();
                storageFilesSize = storageFilesSize.add(storageStorageMonitoringItem.getFilesSize());
                storageFoldersCount = storageFoldersCount + storageStorageMonitoringItem.getFoldersCount();
                storageItemsCount = storageItemsCount + storageStorageMonitoringItem.getItemsCount();
                if (Objects.nonNull(storageStorageMonitoringItem.getArtifactsDownloadedCount())) {
                    storageArtifactsDownloadedCount = storageArtifactsDownloadedCount + storageStorageMonitoringItem.getArtifactsDownloadedCount();
                }
                if (StringUtils.isBlank(storageStorageMonitoring.getStorageDeviceName())) {
                    storageStorageMonitoring.setStorageDeviceName(storageStorageMonitoringItem.getStorageDeviceName());
                }
                if (StringUtils.isBlank(storageStorageMonitoring.getStorageDeviceType())) {
                    storageStorageMonitoring.setStorageDeviceType(storageStorageMonitoringItem.getStorageDeviceType());
                }
                if (Objects.isNull(storageStorageMonitoring.getStorageDeviceSize())) {
                    storageStorageMonitoring.setStorageDeviceSize(storageStorageMonitoringItem.getStorageDeviceSize());
                }
                if (Objects.isNull(storageStorageMonitoring.getStorageQuotaSize())) {
                    storageStorageMonitoring.setStorageQuotaSize(storageStorageMonitoringItem.getStorageQuotaSize());
                }
                if (StringUtils.isBlank(storageStorageMonitoring.getStorageProvider())) {
                    storageStorageMonitoring.setStorageProvider(storageStorageMonitoringItem.getStorageProvider());
                }
            }
            storageStorageMonitoring.setRepositoryCount((int) storageEntryStorageMonitoringList.stream().filter(item -> DirectoryDataTypeEnum.REPOSITORY.getType().equals(item.getDataType())).count());
            storageStorageMonitoring.setArtifactsCount(storageArtifactsCount);
            storageStorageMonitoring.setArtifactsSize(storageArtifactsSize);
            storageStorageMonitoring.setFilesCount(storageFilesCount);
            storageStorageMonitoring.setFilesSize(storageFilesSize);
            storageStorageMonitoring.setFoldersCount(storageFoldersCount);
            storageStorageMonitoring.setItemsCount(storageItemsCount);
            storageStorageMonitoring.setArtifactsDownloadedCount(storageArtifactsDownloadedCount);
            storageMonitoringList.add(storageStorageMonitoring);
        }
    }

    private void calculatorRepository(List<StorageMonitoring> storageMonitoringList, Map<String, StorageDevice> storageDeviceMap, Storage storage, Repository repository, RepositoryPath repositoryPath) {
        //计算目录相关信息
        DirectorySizeCalculatorUtils directorySizeCalculatorUtils = new DirectorySizeCalculatorUtils(repositoryPath);
        Result result = directorySizeCalculatorUtils.compute();
        long itemsCount = result.getArtifactsCount() + result.getDirectoriesCount(), artifactsDownloadedCount = 0;
        Date date = new Date();
        StorageDevice storageDevice = storageDeviceMap.get(storage.getId());
        Long storageMaxSize = 0L;

        BigDecimal artifactsSize, filesSize, storageQuotaSize = BigDecimal.valueOf(storageMaxSize), usedStorageQuotaSizePercentage = BigDecimal.ZERO, storageDeviceSize = BigDecimal.valueOf(storageDevice.getTotalSpace()), usedStorageDeviceSizePercentage = BigDecimal.ONE;
        artifactsSize = BigDecimal.valueOf(result.getTotalArtifactsSize());
        filesSize = BigDecimal.valueOf(result.getTotalFilesSize());
        if (storageQuotaSize.compareTo(BigDecimal.ZERO) > 0) {
            usedStorageQuotaSizePercentage = artifactsSize.divide(storageQuotaSize, 4, RoundingMode.HALF_UP).multiply(BigDecimal.valueOf(100));
        }
        usedStorageDeviceSizePercentage = artifactsSize.divide(storageDeviceSize, 4, RoundingMode.HALF_UP).multiply(BigDecimal.valueOf(100));
        artifactsDownloadedCount = artifactRepository.sumDownloadCountByStorageIdAndRepositoryId(Lists.newArrayList(repositoryPath.getStorageId() + "-" + repositoryPath.getRepositoryId()));
        //仓库根目录相关数据
        StorageMonitoring storageMonitoring = StorageMonitoring.builder().id(idGenerateUtils.generateId("storageMonitoringId")).storageId(repositoryPath.getStorageId()).repositoryId(repositoryPath.getRepositoryId()).repositoryType(repository.getType()).repositoryLayout(repository.getLayout()).repositorySubLayout(repository.getSubLayout()).artifactsDownloadedCount(artifactsDownloadedCount).artifactsSize(artifactsSize).artifactsCount(result.getArtifactsCount()).filesCount(result.getFilesCount())
                .filesSize(filesSize).foldersCount(result.getDirectoriesCount()).createTime(date).dataType(DirectoryDataTypeEnum.REPOSITORY.getType()).itemsCount(itemsCount).storagePath(repositoryPath.toString()).isLatest(Boolean.TRUE).updateTime(date).storageQuotaSize(storageQuotaSize).usedStorageQuotaSizePercentage(usedStorageQuotaSizePercentage).storageProvider(storage.getStorageProvider()).storageDeviceName(storageDevice.getName())
                .storageDeviceSize(storageDeviceSize).usedStorageDeviceSizePercentage(usedStorageDeviceSizePercentage).storageDeviceType(storageDevice.getType()).build();
        storageMonitoringList.add(storageMonitoring);
    }

    private List<String> getIncludeRepositories() {
        String cacheKey = "STORAGE_MONITORING_INCLUDE_REPOSITORIES";
        String cacheValue = distributedCacheComponent.get(cacheKey);
        if (StringUtils.isNotBlank(cacheValue)) {
            return Arrays.asList(cacheValue.split(","));
        }
        return null;
    }

    private List<String> getExcludeRepositories() {
        String cacheKey = "STORAGE_MONITORING_EXCLUDE_REPOSITORIES";
        String cacheValue = distributedCacheComponent.get(cacheKey);
        if (StringUtils.isNotBlank(cacheValue)) {
            return Arrays.asList(cacheValue.split(","));
        }
        return null;
    }

}
