package com.veadan.folib.services.impl;

import cn.hutool.core.date.DateUtil;
import cn.hutool.core.util.StrUtil;
import com.beust.jcommander.internal.Sets;
import com.github.pagehelper.Page;
import com.github.pagehelper.PageHelper;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.veadan.folib.cloud.storage.s3fs.S3Path;
import com.veadan.folib.components.IdGenerateUtils;
import com.veadan.folib.configuration.ConfigurationManager;
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
import tk.mybatis.mapper.entity.Example;

import javax.inject.Inject;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.file.FileStore;
import java.nio.file.Files;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.ForkJoinPool;
import java.util.stream.Collectors;

/**
 * @author leipenghui
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

    @Inject
    @Lazy
    private IdGenerateUtils idGenerateUtils;

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateStorageMonitoringData() {
        log.info("Update storage monitoring data start [{}]", DateUtil.now());
        long start = System.currentTimeMillis();
        Map<String, Storage> storageMap = getStorageMap();
        ForkJoinPool pool = new ForkJoinPool();
        List<StorageMonitoring> storageMonitoringList = Lists.newArrayList(), trashStorageMonitoringList = Lists.newArrayList();
        List<StorageDevice> storageDeviceList = Lists.newArrayList();
        Map<String, StorageDevice> storageDeviceMap = Maps.newConcurrentMap();
        handlerStorage(storageMap, pool, storageMonitoringList, trashStorageMonitoringList, storageDeviceList, storageDeviceMap);
        Date date = new Date();
        storageStorageMonitoring(storageMonitoringList, date);
        platformStorageMonitoring(storageMap, storageMonitoringList, date);
        storageDeviceStorageMonitoring(storageMonitoringList, storageDeviceList, date);
        storageMonitoringList = trashStorageMonitoring(storageMonitoringList, trashStorageMonitoringList, date);
        Example deleteExample = Example.builder(StorageMonitoring.class).build();
        Example.Criteria deleteCriteria = deleteExample.createCriteria();
        LocalDateTime deleteDeadlineDate = LocalDateTime.now().minusDays(60);
        deleteCriteria.andLessThanOrEqualTo("createTime", DateUtil.formatLocalDateTime(deleteDeadlineDate));
        storageMonitoringMapper.deleteByExample(deleteExample);
        Example example = Example.builder(StorageMonitoring.class).build();
        Example.Criteria criteria = example.createCriteria();
        criteria.andEqualTo("isLatest", true);
        storageMonitoringMapper.updateByExampleSelective(StorageMonitoring.builder().isLatest(Boolean.FALSE).build(), example);
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
            storageMonitoringMapper.insertSelective(storageMonitoring);
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateStorageMonitoring(StorageMonitoring storageMonitoring) {
        StorageMonitoringRes storageMonitoringRes = queryOneStorageMonitoring(storageMonitoring);
        if (Objects.nonNull(storageMonitoringRes)) {
            Example example = Example.builder(StorageMonitoring.class).build();
            Example.Criteria criteria = example.createCriteria();
            criteria.andEqualTo("id", storageMonitoring.getId());
            storageMonitoringMapper.updateByExampleSelective(storageMonitoring, example);
        }
    }

    @Override
    public List<StorageMonitoringRes> queryStorageMonitoring(StorageMonitoring storageMonitoring) {
        Example example = Example.builder(StorageMonitoring.class).build();
        Example.Criteria criteria = example.createCriteria();
        if (Objects.nonNull(storageMonitoring)) {
            if (StringUtils.isNotBlank(storageMonitoring.getStorageId())) {
                criteria.andEqualTo("storageId", storageMonitoring.getStorageId());
            }
            if (StringUtils.isNotBlank(storageMonitoring.getRepositoryId())) {
                criteria.andEqualTo("repositoryId", storageMonitoring.getRepositoryId());
            }
            if (StringUtils.isNotBlank(storageMonitoring.getRepositoryType())) {
                criteria.andEqualTo("repositoryType", storageMonitoring.getRepositoryType());
            }
            if (StringUtils.isNotBlank(storageMonitoring.getRepositoryLayout())) {
                criteria.andEqualTo("repositoryLayout", storageMonitoring.getRepositoryLayout());
            }
            if (Objects.nonNull(storageMonitoring.getDataType())) {
                criteria.andEqualTo("dataType", storageMonitoring.getDataType());
            }
            if (Objects.nonNull(storageMonitoring.getIsLatest())) {
                criteria.andEqualTo("isLatest", storageMonitoring.getIsLatest());
            }
        }
        List<StorageMonitoringRes> storageMonitoringResList = null;
        List<StorageMonitoring> storageMonitoringList = storageMonitoringMapper.selectByExample(example);
        if (CollectionUtils.isNotEmpty(storageMonitoringList)) {
            storageMonitoringResList = storageMonitoringList.stream().map(item -> {
                StorageMonitoringRes storageMonitoringRes = StorageMonitoringRes.builder().build();
                BeanUtils.copyProperties(item, storageMonitoringRes);
                storageMonitoringRes.setCreateDate(DateUtil.formatDate(storageMonitoringRes.getCreateTime()));
                return storageMonitoringRes;
            }).collect(Collectors.toList());
        }
        return storageMonitoringResList;
    }

    @Override
    public StorageMonitoringRes queryOneStorageMonitoring(StorageMonitoring storageMonitoring) {
        Example example = Example.builder(StorageMonitoring.class).build();
        Example.Criteria criteria = example.createCriteria();
        criteria.andEqualTo("id", storageMonitoring.getId());
        StorageMonitoringRes storageMonitoringRes = null;
        StorageMonitoring dbStorageMonitoring = storageMonitoringMapper.selectOneByExample(example);
        if (Objects.nonNull(dbStorageMonitoring)) {
            storageMonitoringRes = StorageMonitoringRes.builder().build();
            BeanUtils.copyProperties(dbStorageMonitoring, storageMonitoringRes);
            storageMonitoringRes.setCreateDate(DateUtil.formatDate(storageMonitoringRes.getCreateTime()));
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
        Example example = Example.builder(StorageMonitoring.class).build();
        Example.Criteria criteria = example.createCriteria();
        if (StringUtils.isNotBlank(storageMonitoringReq.getStorageId())) {
            criteria.andEqualTo("storageId", storageMonitoringReq.getStorageId());
        }
        if (StringUtils.isNotBlank(storageMonitoringReq.getRepositoryId())) {
            criteria.andEqualTo("repositoryId", storageMonitoringReq.getRepositoryId());
        }
        if (StringUtils.isNotBlank(storageMonitoringReq.getRepositoryType())) {
            criteria.andEqualTo("repositoryType", storageMonitoringReq.getRepositoryType());
        }
        if (StringUtils.isNotBlank(storageMonitoringReq.getRepositoryLayout())) {
            criteria.andEqualTo("repositoryLayout", storageMonitoringReq.getRepositoryLayout());
        }
        if (StringUtils.isNotBlank(storageMonitoringReq.getRepositorySubLayout())) {
            criteria.andEqualTo("repositorySubLayout", storageMonitoringReq.getRepositorySubLayout());
        }
        if (Objects.nonNull(storageMonitoringReq.getDataType())) {
            criteria.andEqualTo("dataType", storageMonitoringReq.getDataType());
        }
        if (CollectionUtils.isNotEmpty(storageMonitoringReq.getDataTypes())) {
            criteria.andIn("dataType", storageMonitoringReq.getDataTypes());
        }
        if (Objects.nonNull(storageMonitoringReq.getIsLatest())) {
            criteria.andEqualTo("isLatest", storageMonitoringReq.getIsLatest());
        }
        if (StringUtils.isNotBlank(storageMonitoringReq.getStorageDeviceName())) {
            criteria.andEqualTo("storageDeviceName", storageMonitoringReq.getStorageDeviceName());
        }
        StringBuilder orderBy = new StringBuilder();
        orderBy.append("data_type desc");
        if (StringUtils.isNotBlank(storageMonitoringReq.getSortField()) && StringUtils.isNotBlank(storageMonitoringReq.getSortOrder())) {
            orderBy.append(",");
            orderBy.append(StrUtil.toUnderlineCase(storageMonitoringReq.getSortField()));
            if (Paginator.Order.ASC.toString().equalsIgnoreCase(storageMonitoringReq.getSortOrder())) {
                orderBy.append(" asc");
            } else if (Paginator.Order.DESC.toString().equalsIgnoreCase(storageMonitoringReq.getSortOrder())) {
                orderBy.append(" desc");
            }
        }
        example.setOrderByClause(orderBy.toString());
        Page<Object> result = PageHelper.startPage(page, limit);
        List<StorageMonitoring> storageMonitoringList = storageMonitoringMapper.selectByExample(example);
        List<StorageMonitoringRes> storageMonitoringResList = null;
        if (CollectionUtils.isNotEmpty(storageMonitoringList)) {
            storageMonitoringResList = storageMonitoringList.stream().map(item -> {
                StorageMonitoringRes storageMonitoringRes = StorageMonitoringRes.builder().build();
                BeanUtils.copyProperties(item, storageMonitoringRes);
                storageMonitoringRes.setCreateDate(DateUtil.formatDate(storageMonitoringRes.getCreateTime()));
                return storageMonitoringRes;
            }).collect(Collectors.toList());
        }
        return new TableResultResponse<StorageMonitoringRes>(result.getTotal(), storageMonitoringResList);
    }

    private Map<String, Storage> getStorageMap() {
        return configurationManager.getConfiguration().getStorages();
    }

    private void handlerStorage(Map<String, Storage> storageMap, ForkJoinPool pool, List<StorageMonitoring> storageMonitoringList, List<StorageMonitoring> trashStorageMonitoringList, List<StorageDevice> storageDeviceList, Map<String, StorageDevice> storageDeviceMap) {
        Storage storage;
        Repository repository;
        RootRepositoryPath rootRepositoryPath;
        FileStore fileStore;
        StorageDevice storageDevice;
        StorageMonitoring storageMonitoring;
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
                    fileStore = Files.getFileStore(rootRepositoryPath);
                    long usedSpace = fileStore.getTotalSpace() - fileStore.getUsableSpace();
                    storageDevice = StorageDevice.builder().name(fileStore.name()).totalSpace(fileStore.getTotalSpace()).usableSpace(fileStore.getUsableSpace()).usedSpace(usedSpace).build();
                    storageDevice.setType(StorageDeviceTypeEnum.NAS.getType());
                    if (rootRepositoryPath.getTarget() instanceof S3Path) {
                        storageDevice.setType(StorageDeviceTypeEnum.S3.getType());
                    }
                    if (!storageDeviceMap.containsKey(storageId)) {
                        storageDeviceMap.put(storageId, storageDevice);
                    }
                    if (!storageDeviceList.contains(storageDevice)) {
                        storageDeviceList.add(storageDevice);
                    }
                    calculatorRepository(pool, storageMonitoringList, trashStorageMonitoringList, storageDeviceMap, storage, repository, rootRepositoryPath);
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
            repositoryStorageMonitoringList = storageMonitoringList.stream().filter(item -> (DirectoryDataTypeEnum.REPOSITORY.getType().equals(item.getDataType()) || DirectoryDataTypeEnum.TRASH.getType().equals(item.getDataType())) && itemStorageDevice.getName().equals(item.getStorageDeviceName())).collect(Collectors.toList());

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
            if (!DirectoryDataTypeEnum.REPOSITORY.getType().equals(itemStorageMonitoring.getDataType()) && !DirectoryDataTypeEnum.TRASH.getType().equals(itemStorageMonitoring.getDataType())) {
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

    private List<StorageMonitoring> trashStorageMonitoring(List<StorageMonitoring> storageMonitoringList, List<StorageMonitoring> trashStorageMonitoringList, Date date) {
        storageMonitoringList = storageMonitoringList.stream().filter(item -> !DirectoryDataTypeEnum.TRASH.getType().equals(item.getDataType())).collect(Collectors.toList());

        BigDecimal trashArtifactsSize = BigDecimal.ZERO, trashFilesSize = BigDecimal.ZERO;
        long trashArtifactsCount = 0L, trashFilesCount = 0L, trashFoldersCount = 0L, trashItemsCount = 0L;
        //回收站级别相关数据统计
        StorageMonitoring trashStorageMonitoring = StorageMonitoring.builder().id(idGenerateUtils.generateId("storageMonitoringId")).dataType(DirectoryDataTypeEnum.TRASH.getType()).isLatest(Boolean.TRUE).updateTime(date).build();
        for (StorageMonitoring itemStorageMonitoring : trashStorageMonitoringList) {
            trashArtifactsCount = trashArtifactsCount + itemStorageMonitoring.getArtifactsCount();
            trashArtifactsSize = trashArtifactsSize.add(itemStorageMonitoring.getArtifactsSize());
            trashFilesCount = trashFilesCount + itemStorageMonitoring.getFilesCount();
            trashFilesSize = trashFilesSize.add(itemStorageMonitoring.getFilesSize());
            trashFoldersCount = trashFoldersCount + itemStorageMonitoring.getFoldersCount();
            trashItemsCount = trashItemsCount + itemStorageMonitoring.getItemsCount();
        }
        trashStorageMonitoring.setArtifactsCount(trashArtifactsCount);
        trashStorageMonitoring.setArtifactsSize(trashArtifactsSize);
        trashStorageMonitoring.setFilesCount(trashFilesCount);
        trashStorageMonitoring.setFilesSize(trashFilesSize);
        trashStorageMonitoring.setFoldersCount(trashFoldersCount);
        trashStorageMonitoring.setItemsCount(trashItemsCount);
        storageMonitoringList.add(trashStorageMonitoring);
        return storageMonitoringList;
    }

    private void calculatorRepository(ForkJoinPool pool, List<StorageMonitoring> storageMonitoringList, List<StorageMonitoring> trashStorageMonitoringList, Map<String, StorageDevice> storageDeviceMap, Storage storage, Repository repository, RepositoryPath repositoryPath) {
        //计算目录相关信息
        DirectorySizeCalculatorUtils directorySizeCalculatorUtils = new DirectorySizeCalculatorUtils(repositoryPath);
        Result result = pool.invoke(directorySizeCalculatorUtils);
        long itemsCount = result.getArtifactsCount() + result.getDirectoriesCount(), trashItemsCount = result.getTrashArtifactsCount() + result.getTrashDirectoriesCount(), artifactsDownloadedCount = 0;
        Date date = new Date();
        StorageDevice storageDevice = storageDeviceMap.get(storage.getId());
        BigDecimal artifactsSize, filesSize, trashArtifactsSize, trashFilesSize, storageQuotaSize = BigDecimal.valueOf(storage.getStorageMaxSize()), usedStorageQuotaSizePercentage = BigDecimal.ZERO, storageDeviceSize = BigDecimal.valueOf(storageDevice.getTotalSpace()), usedStorageDeviceSizePercentage = BigDecimal.ONE;
        artifactsSize = BigDecimal.valueOf(result.getTotalArtifactsSize());
        filesSize = BigDecimal.valueOf(result.getTotalFilesSize());
        if (storageQuotaSize.compareTo(BigDecimal.ZERO) > 0) {
            usedStorageQuotaSizePercentage = artifactsSize.divide(storageQuotaSize, 4, RoundingMode.HALF_UP).multiply(BigDecimal.valueOf(100));
        }
        usedStorageDeviceSizePercentage = artifactsSize.divide(storageDeviceSize, 4, RoundingMode.HALF_UP).multiply(BigDecimal.valueOf(100));
        artifactsDownloadedCount = artifactRepository.sumDownloadCountByStorageIdAndRepositoryId(Lists.newArrayList(repositoryPath.getStorageId() + "-" + repositoryPath.getRepositoryId()));
        //仓库根目录相关数据，不包含回收站
        StorageMonitoring storageMonitoring = StorageMonitoring.builder().id(idGenerateUtils.generateId("storageMonitoringId")).storageId(repositoryPath.getStorageId()).repositoryId(repositoryPath.getRepositoryId()).repositoryType(repository.getType()).repositoryLayout(repository.getLayout()).repositorySubLayout(repository.getSubLayout()).artifactsDownloadedCount(artifactsDownloadedCount).artifactsSize(artifactsSize).artifactsCount(result.getArtifactsCount()).filesCount(result.getFilesCount())
                .filesSize(filesSize).foldersCount(result.getDirectoriesCount()).createTime(date).dataType(DirectoryDataTypeEnum.REPOSITORY.getType()).itemsCount(itemsCount).storagePath(repositoryPath.toString()).isLatest(Boolean.TRUE).updateTime(date).storageQuotaSize(storageQuotaSize).usedStorageQuotaSizePercentage(usedStorageQuotaSizePercentage).storageProvider(storage.getStorageProvider()).storageDeviceName(storageDevice.getName())
                .storageDeviceSize(storageDeviceSize).usedStorageDeviceSizePercentage(usedStorageDeviceSizePercentage).storageDeviceType(storageDevice.getType()).build();
        storageMonitoringList.add(storageMonitoring);

        trashArtifactsSize = BigDecimal.valueOf(result.getTrashTotalArtifactsSize());
        trashFilesSize = BigDecimal.valueOf(result.getTrashTotalFilesSize());
        //回收站目录相关数据
        StorageMonitoring trashStorageMonitoring = StorageMonitoring.builder().id(idGenerateUtils.generateId("storageMonitoringId")).storageId(repositoryPath.getStorageId()).repositoryId(repositoryPath.getRepositoryId()).repositoryType(repository.getType()).repositoryLayout(repository.getLayout()).repositorySubLayout(repository.getSubLayout()).artifactsSize(trashArtifactsSize).artifactsCount(result.getTrashArtifactsCount()).filesCount(result.getTrashFilesCount())
                .filesSize(trashFilesSize).foldersCount(result.getTrashDirectoriesCount()).createTime(date).dataType(DirectoryDataTypeEnum.TRASH.getType()).itemsCount(trashItemsCount).storagePath(repositoryPath.toString()).isLatest(Boolean.TRUE).updateTime(date).storageQuotaSize(storageQuotaSize).storageProvider(storage.getStorageProvider()).storageDeviceName(storageDevice.getName())
                .storageDeviceSize(storageDeviceSize).storageDeviceType(storageDevice.getType()).build();

        storageMonitoringList.add(trashStorageMonitoring);
        trashStorageMonitoringList.add(trashStorageMonitoring);
    }

}
