package com.veadan.folib.services.impl;

import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.github.pagehelper.Page;
import com.github.pagehelper.PageHelper;
import com.google.common.collect.Lists;
import com.hazelcast.core.HazelcastInstance;
import com.veadan.folib.components.IdGenerateUtils;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.configuration.ConfigurationUtils;
import com.veadan.folib.domain.PackageNameBlockInfo;
import com.veadan.folib.domain.blockstrategy.BlockStrategyRecord;
import com.veadan.folib.entity.*;
import com.veadan.folib.forms.blockstrategy.BlockStrategyForm;
import com.veadan.folib.forms.packagenameblock.PackageNameBlockForm;
import com.veadan.folib.mapper.BlockStrategyInfoMapper;
import com.veadan.folib.mapper.BlockStrategyMapper;
import com.veadan.folib.mapper.BlockStrategyRepositoryMapper;
import com.veadan.folib.scanner.common.msg.TableResultResponse;
import com.veadan.folib.services.BlockStrategyService;
import com.veadan.folib.services.LicenseService;
import com.veadan.folib.services.PackageNameBlockService;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.utils.UserUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

/**
 * @author leipenghui
 **/
@Slf4j
@Service
public class BlockStrategyServiceImpl implements BlockStrategyService {

    private final static String REPOSITORY_BLOCK_STRATEGY_KEY = "REPOSITORY_BLOCK_STRATEGY_KEY";

    @Autowired
    private BlockStrategyMapper blockStrategyMapper;

    @Autowired
    private BlockStrategyRepositoryMapper blockStrategyRepositoryMapper;

    @Autowired
    private BlockStrategyInfoMapper blockStrategyInfoMapper;

    @Autowired
    @Lazy
    private PackageNameBlockService packageNameBlockService;

    @Autowired
    @Lazy
    private LicenseService licenseService;

    @Autowired
    private ConfigurationManager configurationManager;

    @Autowired
    private IdGenerateUtils idGenerateUtils;

    @Autowired
    private HazelcastInstance hazelcastInstance;

    @Override
    public TableResultResponse<BlockStrategyRecord> queryBlockStrategyPage(Integer page, Integer limit, BlockStrategyForm blockStrategyForm) {
        if (Objects.isNull(page)) {
            page = 1;
        }
        if (Objects.isNull(limit)) {
            limit = 10;
        }
        Page<Object> result = PageHelper.startPage(page, limit);
        List<BlockStrategy> blockStrategyList = blockStrategyMapper.selectBlockList(blockStrategyForm);
        return new TableResultResponse<BlockStrategyRecord>(result.getTotal(), Optional.ofNullable(blockStrategyList).orElse(Collections.emptyList()).stream().map(blockStrategy -> {
            BlockStrategyRecord blockStrategyRecord = BlockStrategyRecord.builder().build();
            BeanUtils.copyProperties(blockStrategy, blockStrategyRecord);
            blockStrategyRecord.setId(blockStrategy.getId().toString());
            return blockStrategyRecord;
        }).collect(Collectors.toList()));
    }

    @Override
    public List<BlockStrategyRecord> queryBlockStrategyList(BlockStrategyForm blockStrategyForm) {
        List<BlockStrategyRecord> blockStrategyList = blockStrategyMapper.selectInfoList(blockStrategyForm);
        return blockStrategyList;
    }

    @Override
    public BlockStrategyForm queryBlockStrategy(BlockStrategy blockStrategy) {
        BlockStrategyForm blockStrategyForm = null;
        String blockStrategyName = blockStrategy.getBlockStrategyName();
        BlockStrategy existsBlockStrategy = getBlockStrategy(BlockStrategy.builder().blockStrategyName(blockStrategyName).build());
        if (Objects.isNull(existsBlockStrategy)) {
            return null;
        }
        blockStrategyForm = BlockStrategyForm.builder().build();
        BeanUtils.copyProperties(existsBlockStrategy, blockStrategyForm);
        blockStrategyForm.setId(existsBlockStrategy.getId().toString());
        if (StringUtils.isNotBlank(existsBlockStrategy.getVulnerabilityLevels())) {
            blockStrategyForm.setVulnerabilityLevels(Arrays.asList(existsBlockStrategy.getVulnerabilityLevels().split(",")));
        }
        //查询仓库
        List<BlockStrategyRepository> blockStrategyRepositories = blockStrategyRepositoryMapper.selectList(Wrappers.<BlockStrategyRepository>lambdaQuery()
                .eq(BlockStrategyRepository::getBlockStrategyId,existsBlockStrategy.getId()));

        blockStrategyForm.setRepositories(Optional.ofNullable(blockStrategyRepositories).orElse(Collections.emptyList()).stream().map(blockStrategyRepository -> ConfigurationUtils.getStorageIdAndRepositoryId(blockStrategyRepository.getStorageId(), blockStrategyRepository.getRepositoryId())).collect(Collectors.toList()));
        //查询包名、license
        List<BlockStrategyInfo> blockStrategyInfos = blockStrategyInfoMapper.selectList(Wrappers.<BlockStrategyInfo>lambdaQuery()
                .eq(BlockStrategyInfo::getBlockStrategyId,existsBlockStrategy.getId()));
        List<String> packageNames = Lists.newArrayList(), licenses = Lists.newArrayList();
        if (CollectionUtils.isNotEmpty(blockStrategyInfos)) {
            blockStrategyInfos.forEach(blockStrategyInfo -> {
                if (StringUtils.isNotBlank(blockStrategyInfo.getLicenseId())) {
                    licenses.add(blockStrategyInfo.getLicenseId());
                } else if (StringUtils.isNotBlank(blockStrategyInfo.getPackageName())) {
                    packageNames.add(blockStrategyInfo.getPackageName());
                }
            });
        }
        blockStrategyForm.setPackageNames(packageNames);
        blockStrategyForm.setLicenses(licenses);
        return blockStrategyForm;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void saveBlockStrategy(BlockStrategyForm blockStrategyForm) {
        String blockStrategyName = blockStrategyForm.getBlockStrategyName();
        BlockStrategy existsBlockStrategy = getBlockStrategy(BlockStrategy.builder().blockStrategyName(blockStrategyName).build());
        if (Objects.nonNull(existsBlockStrategy)) {
            return;
        }
        String username = UserUtils.getUsername();
        Date now = new Date();
        Long blockStrategyId = idGenerateUtils.generateId("blockStrategyId");
        BlockStrategy blockStrategy = BlockStrategy.builder().blockStrategyName(blockStrategyName).createBy(username).createTime(now)
                .filterVulnerabilityWhites(false).filterVulnerabilityBlacks(false).filterAllPackageName(false).filterAllLicense(false)
                .filterLicenseWhites(false).filterLicenseBlacks(false).id(blockStrategyId).build();
        if (Boolean.TRUE.equals(blockStrategyForm.getFilterVulnerabilityWhites())) {
            blockStrategy.setFilterVulnerabilityWhites(true);
        }
        if (Boolean.TRUE.equals(blockStrategyForm.getFilterVulnerabilityBlacks())) {
            blockStrategy.setFilterVulnerabilityBlacks(true);
        }
        if (Boolean.TRUE.equals(blockStrategyForm.getFilterAllPackageName())) {
            blockStrategy.setFilterAllPackageName(true);
        }
        if (Boolean.TRUE.equals(blockStrategyForm.getFilterAllLicense())) {
            blockStrategy.setFilterAllLicense(true);
        }
        if (Boolean.TRUE.equals(blockStrategyForm.getFilterLicenseWhites())) {
            blockStrategy.setFilterLicenseWhites(true);
        }
        if (Boolean.TRUE.equals(blockStrategyForm.getFilterLicenseBlacks())) {
            blockStrategy.setFilterLicenseBlacks(true);
        }
        if (CollectionUtils.isNotEmpty(blockStrategyForm.getVulnerabilityLevels())) {
            blockStrategy.setVulnerabilityLevels(String.join(",", blockStrategyForm.getVulnerabilityLevels()));
        }
        blockStrategyMapper.insert(blockStrategy);
        if (CollectionUtils.isNotEmpty(blockStrategyForm.getRepositories())) {
            String storageId = "", repositoryId = "";
            Storage storage = null;
            Repository repository = null;
            List<BlockStrategyRepository> blockStrategyRepositoryList = Lists.newArrayList();
            BlockStrategyRepository blockStrategyRepository = null;
            for (String storageIdAndRepositoryId : blockStrategyForm.getRepositories()) {
                storageId = ConfigurationUtils.getStorageId("", storageIdAndRepositoryId);
                repositoryId = ConfigurationUtils.getRepositoryId(storageIdAndRepositoryId);
                storage = configurationManager.getStorage(storageId);
                if (Objects.isNull(storage)) {
                    continue;
                }
                repository = storage.getRepository(repositoryId);
                if (Objects.isNull(repository)) {
                    continue;
                }
                blockStrategyRepository = BlockStrategyRepository.builder().id(idGenerateUtils.generateId("blockStrategyRepositoryId"))
                        .blockStrategyId(blockStrategyId).createBy(username).createTime(now).storageId(storageId).repositoryId(repositoryId).build();
                blockStrategyRepositoryList.add(blockStrategyRepository);
                clearCache(storageId, repositoryId);
            }
            if (CollectionUtils.isNotEmpty(blockStrategyRepositoryList)) {
                List<List<BlockStrategyRepository>> blockStrategyRepositories = Lists.partition(blockStrategyRepositoryList, 100);
                for (List<BlockStrategyRepository> itemList : blockStrategyRepositories) {
                    blockStrategyRepositoryMapper.batchInsertBlockStrategyRepository(itemList);
                }
            }
        }
        List<BlockStrategyInfo> blockStrategyInfoList = Lists.newArrayList();
        if (CollectionUtils.isNotEmpty(blockStrategyForm.getPackageNames())) {
            PackageNameBlockInfo packageNameBlockInfo = null;
            BlockStrategyInfo blockStrategyInfo = null;
            for (String packageName : blockStrategyForm.getPackageNames()) {
                packageNameBlockInfo = packageNameBlockService.selectOnePackageNameBlock(PackageNameBlockForm.builder().packageName(packageName).build());
                if (Objects.isNull(packageNameBlockInfo)) {
                    continue;
                }
                blockStrategyInfo = BlockStrategyInfo.builder().id(idGenerateUtils.generateId("blockStrategyInfoId"))
                        .blockStrategyId(blockStrategyId).createBy(username).createTime(now).packageName(packageName).build();
                blockStrategyInfoList.add(blockStrategyInfo);
            }
        }
        if (CollectionUtils.isNotEmpty(blockStrategyForm.getLicenses())) {
            License license = null;
            BlockStrategyInfo blockStrategyInfo = null;
            for (String licenseId : blockStrategyForm.getLicenses()) {
                license = licenseService.selectOneLicense(License.builder().licenseId(licenseId).build());
                if (Objects.isNull(license)) {
                    continue;
                }
                blockStrategyInfo = BlockStrategyInfo.builder().id(idGenerateUtils.generateId("blockStrategyInfoId"))
                        .blockStrategyId(blockStrategyId).createBy(username).createTime(now).licenseId(licenseId).build();
                blockStrategyInfoList.add(blockStrategyInfo);
            }
        }
        if (CollectionUtils.isNotEmpty(blockStrategyInfoList)) {
            List<List<BlockStrategyInfo>> blockStrategyInfos = Lists.partition(blockStrategyInfoList, 100);
            for (List<BlockStrategyInfo> itemList : blockStrategyInfos) {
                blockStrategyInfoMapper.batchInsertBlockStrategyInfo(itemList);
            }
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateBlockStrategy(BlockStrategyForm blockStrategyForm) {
        String blockStrategyName = blockStrategyForm.getBlockStrategyName();
        BlockStrategy existsBlockStrategy = null;
        if (StringUtils.isNotBlank(blockStrategyForm.getId())) {
            existsBlockStrategy = getBlockStrategy(BlockStrategy.builder().id(Long.parseLong(blockStrategyForm.getId())).build());
        } else {
            existsBlockStrategy = getBlockStrategy(BlockStrategy.builder().blockStrategyName(blockStrategyName).build());
        }
        if (Objects.isNull(existsBlockStrategy)) {
            return;
        }
        String username = UserUtils.getUsername();
        Date now = new Date();
        Long blockStrategyId = existsBlockStrategy.getId();
        BlockStrategy blockStrategy = BlockStrategy.builder().blockStrategyName(blockStrategyName).updateBy(username).updateTime(now)
                .filterVulnerabilityWhites(false).filterVulnerabilityBlacks(false).filterAllPackageName(false).filterAllLicense(false)
                .filterLicenseWhites(false).filterLicenseBlacks(false).id(blockStrategyId).build();
        if (Boolean.TRUE.equals(blockStrategyForm.getFilterVulnerabilityWhites())) {
            blockStrategy.setFilterVulnerabilityWhites(true);
        }
        if (Boolean.TRUE.equals(blockStrategyForm.getFilterVulnerabilityBlacks())) {
            blockStrategy.setFilterVulnerabilityBlacks(true);
        }
        if (Boolean.TRUE.equals(blockStrategyForm.getFilterAllPackageName())) {
            blockStrategy.setFilterAllPackageName(true);
        }
        if (Boolean.TRUE.equals(blockStrategyForm.getFilterAllLicense())) {
            blockStrategy.setFilterAllLicense(true);
        }
        if (Boolean.TRUE.equals(blockStrategyForm.getFilterLicenseWhites())) {
            blockStrategy.setFilterLicenseWhites(true);
        }
        if (Boolean.TRUE.equals(blockStrategyForm.getFilterLicenseBlacks())) {
            blockStrategy.setFilterLicenseBlacks(true);
        }
        if (CollectionUtils.isNotEmpty(blockStrategyForm.getVulnerabilityLevels())) {
            blockStrategy.setVulnerabilityLevels(String.join(",", blockStrategyForm.getVulnerabilityLevels()));
        }
        blockStrategyMapper.updateById(blockStrategy);
        if (CollectionUtils.isNotEmpty(blockStrategyForm.getRepositories())) {
            String storageId = "", repositoryId = "";
            Storage storage = null;
            Repository repository = null;
            List<BlockStrategyRepository> blockStrategyRepositoryList = Lists.newArrayList();
            BlockStrategyRepository blockStrategyRepository = null;
            for (String storageIdAndRepositoryId : blockStrategyForm.getRepositories()) {
                storageId = ConfigurationUtils.getStorageId("", storageIdAndRepositoryId);
                repositoryId = ConfigurationUtils.getRepositoryId(storageIdAndRepositoryId);
                storage = configurationManager.getStorage(storageId);
                if (Objects.isNull(storage)) {
                    continue;
                }
                repository = storage.getRepository(repositoryId);
                if (Objects.isNull(repository)) {
                    continue;
                }
                blockStrategyRepository = BlockStrategyRepository.builder().id(idGenerateUtils.generateId("blockStrategyRepositoryId"))
                        .blockStrategyId(blockStrategyId).createBy(username).createTime(now).storageId(storageId).repositoryId(repositoryId).build();
                blockStrategyRepositoryList.add(blockStrategyRepository);
                //清理当前传入的仓库缓存
                clearCache(storageId, repositoryId);
            }
            clearBlockStrategyRepositoryCache(existsBlockStrategy.getId());
            blockStrategyRepositoryMapper.delete(Wrappers.<BlockStrategyRepository>lambdaQuery().eq(BlockStrategyRepository::getBlockStrategyId,blockStrategyId));
            if (CollectionUtils.isNotEmpty(blockStrategyRepositoryList)) {
                List<List<BlockStrategyRepository>> blockStrategyRepositories = Lists.partition(blockStrategyRepositoryList, 100);
                for (List<BlockStrategyRepository> itemList : blockStrategyRepositories) {
                    blockStrategyRepositoryMapper.batchInsertBlockStrategyRepository(itemList);
                }
            }
        }
        List<BlockStrategyInfo> blockStrategyInfoList = Lists.newArrayList();
        if (CollectionUtils.isNotEmpty(blockStrategyForm.getPackageNames())) {
            PackageNameBlockInfo packageNameBlockInfo = null;
            BlockStrategyInfo blockStrategyInfo = null;
            for (String packageName : blockStrategyForm.getPackageNames()) {
                packageNameBlockInfo = packageNameBlockService.selectOnePackageNameBlock(PackageNameBlockForm.builder().packageName(packageName).build());
                if (Objects.isNull(packageNameBlockInfo)) {
                    continue;
                }
                blockStrategyInfo = BlockStrategyInfo.builder().id(idGenerateUtils.generateId("blockStrategyInfoId"))
                        .blockStrategyId(blockStrategyId).createBy(username).createTime(now).packageName(packageName).build();
                blockStrategyInfoList.add(blockStrategyInfo);
            }
        }
        if (CollectionUtils.isNotEmpty(blockStrategyForm.getLicenses())) {
            License license = null;
            BlockStrategyInfo blockStrategyInfo = null;
            for (String licenseId : blockStrategyForm.getLicenses()) {
                license = licenseService.selectOneLicense(License.builder().licenseId(licenseId).build());
                if (Objects.isNull(license)) {
                    continue;
                }
                blockStrategyInfo = BlockStrategyInfo.builder().id(idGenerateUtils.generateId("blockStrategyInfoId"))
                        .blockStrategyId(blockStrategyId).createBy(username).createTime(now).licenseId(licenseId).build();
                blockStrategyInfoList.add(blockStrategyInfo);
            }
        }
        blockStrategyInfoMapper.delete(Wrappers.<BlockStrategyInfo>lambdaQuery().eq(BlockStrategyInfo::getBlockStrategyId,blockStrategyId));
        if (CollectionUtils.isNotEmpty(blockStrategyInfoList)) {
            List<List<BlockStrategyInfo>> blockStrategyInfos = Lists.partition(blockStrategyInfoList, 100);
            for (List<BlockStrategyInfo> itemList : blockStrategyInfos) {
                blockStrategyInfoMapper.batchInsertBlockStrategyInfo(itemList);
            }
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void deleteBlockStrategy(BlockStrategy blockStrategy) {
        String blockStrategyName = blockStrategy.getBlockStrategyName();
        BlockStrategy existsBlockStrategy = getBlockStrategy(BlockStrategy.builder().blockStrategyName(blockStrategyName).build());
        if (Objects.isNull(existsBlockStrategy)) {
            return;
        }
        clearBlockStrategyRepositoryCache(existsBlockStrategy.getId());
        Long blockStrategyId = existsBlockStrategy.getId();
        blockStrategyMapper.deleteById(blockStrategyId);
        //删除仓库
        blockStrategyRepositoryMapper.delete(Wrappers.<BlockStrategyRepository>lambdaQuery().eq(BlockStrategyRepository::getBlockStrategyId,blockStrategyId));
        //删除包名、license
        blockStrategyInfoMapper.delete(Wrappers.<BlockStrategyInfo>lambdaQuery().eq(BlockStrategyInfo::getBlockStrategyId,blockStrategyId));
    }

    @Override
    public BlockStrategy getBlockStrategy(BlockStrategy blockStrategy) {
        return  blockStrategyMapper.selectOne(Wrappers.<BlockStrategy>lambdaQuery()
                .eq(blockStrategy.getBlockStrategyName()!=null,BlockStrategy::getBlockStrategyName,blockStrategy.getBlockStrategyName())
                .eq(blockStrategy.getId()!=null,BlockStrategy::getId,blockStrategy.getId())

        );
    }

    /**
     * 刷新缓存
     *
     * @param storageId    存储空间
     * @param repositoryId 仓库
     */
    private void clearCache(String storageId, String repositoryId) {
        String repositoryCacheKey = String.format("%s-%s", storageId, repositoryId);
        Map<String, List<License>> hazelcastMap = hazelcastInstance.getMap(REPOSITORY_BLOCK_STRATEGY_KEY);
        if (MapUtils.isNotEmpty(hazelcastMap)) {
            hazelcastMap.remove(repositoryCacheKey);
            log.info("Clear block strategy cache [{}] [{}]", storageId, repositoryId);
        }
    }

    /**
     * 加入缓存
     *
     * @param key        key
     * @param cacheValue 缓存值
     * @param ttl        缓存时间，小时
     */
    private void putCache(String key, List<BlockStrategyRecord> cacheValue, long ttl) {
        if (CollectionUtils.isEmpty(cacheValue)) {
            return;
        }
        hazelcastInstance.getMap(REPOSITORY_BLOCK_STRATEGY_KEY).put(key, cacheValue, ttl, TimeUnit.HOURS);
    }

    /**
     * 获取缓存
     *
     * @param key key
     * @return 缓存值
     */
    public List<BlockStrategyRecord> getCache(String key) {
        try {
            Map<String, List<BlockStrategyRecord>> hazelcastMap = hazelcastInstance.getMap(REPOSITORY_BLOCK_STRATEGY_KEY);
            if (MapUtils.isEmpty(hazelcastMap)) {
                return null;
            }
            return hazelcastMap.get(key);
        } catch (Exception ex) {
            log.warn(ExceptionUtils.getStackTrace(ex));
            return null;
        }
    }

    @Override
    public List<BlockStrategyRecord> getBlockStrategyRecordCache(String storageId, String repositoryId) {
        String repositoryCacheKey = String.format("%s-%s", storageId, repositoryId);
        List<BlockStrategyRecord> blockStrategyRecordList = getCache(repositoryCacheKey);
        if (CollectionUtils.isEmpty(blockStrategyRecordList)) {
            blockStrategyRecordList = queryBlockStrategyList(BlockStrategyForm.builder().storageId(storageId).repositoryId(repositoryId).build());
            putCache(repositoryCacheKey, blockStrategyRecordList, 8);
        }
        return blockStrategyRecordList;
    }

    /**
     * 清理阻断策略缓存
     *
     * @param id 阻断策略id
     */
    private void clearBlockStrategyRepositoryCache(Long id) {
        List<BlockStrategyRepository> existsBlockStrategyRepositories = blockStrategyRepositoryMapper.selectList(Wrappers.<BlockStrategyRepository>lambdaQuery().eq(BlockStrategyRepository::getBlockStrategyId,id));
        if (CollectionUtils.isNotEmpty(existsBlockStrategyRepositories)) {
            for (BlockStrategyRepository existsBlockStrategyRepository : existsBlockStrategyRepositories) {
                //清理上次保存的仓库缓存
                clearCache(existsBlockStrategyRepository.getStorageId(), existsBlockStrategyRepository.getRepositoryId());
            }
        }
    }

}
