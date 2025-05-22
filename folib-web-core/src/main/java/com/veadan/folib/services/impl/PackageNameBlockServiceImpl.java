package com.veadan.folib.services.impl;

import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.github.pagehelper.Page;
import com.github.pagehelper.PageHelper;
import com.google.common.collect.Lists;
import com.hazelcast.core.HazelcastInstance;
import com.veadan.folib.configuration.MutableSecurityPolicyConfiguration;
import com.veadan.folib.configuration.SecurityPolicyConfiguration;
import com.veadan.folib.domain.PackageNameBlockInfo;
import com.veadan.folib.entity.PackageNameBlock;
import com.veadan.folib.enums.BlockTypeEnum;
import com.veadan.folib.enums.ConditionTypeEnum;
import com.veadan.folib.enums.VersionConditionTypeEnum;
import com.veadan.folib.forms.packagenameblock.PackageNameBlockForm;
import com.veadan.folib.mapper.PackageNameBlockMapper;
import com.veadan.folib.scanner.common.msg.TableResultResponse;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.services.PackageNameBlockService;
import com.veadan.folib.utils.UserUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.inject.Inject;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

/**
 * @author leipenghui
 * @date 2023/10/25
 **/
@Slf4j
@Service
public class PackageNameBlockServiceImpl implements PackageNameBlockService {

    private final static String PACKAGE_NAME_BLOCK = "PACKAGE_NAME_BLOCK";

    private final static String PACKAGE_NAME_BLOCK_CACHE = "PACKAGE_NAME_BLOCK_CACHE";

    @Inject
    private PackageNameBlockMapper packageNameBlockMapper;

    @Inject
    private ConfigurationManagementService configurationManagementService;

    @Inject
    private HazelcastInstance hazelcastInstance;

    @Override
    public TableResultResponse<PackageNameBlockInfo> queryPackageNameBlockList(Integer page, Integer limit, PackageNameBlockForm packageNameBlockForm) {
        if (Objects.isNull(page)) {
            page = 1;
        }
        if (Objects.isNull(limit)) {
            limit = 10;
        }
        Page<Object> result = PageHelper.startPage(page, limit);
        List<PackageNameBlock> packageNameBlockList = packageNameBlockMapper.selectList(Wrappers.<PackageNameBlock>lambdaQuery()
                .like(StringUtils.isNotBlank(packageNameBlockForm.getPackageName()),PackageNameBlock::getPackageName,"%" + packageNameBlockForm.getPackageName() + "%")
                .orderByAsc(PackageNameBlock::getCreateTime)
        );
        return new TableResultResponse<PackageNameBlockInfo>(result.getTotal(), Optional.ofNullable(packageNameBlockList).orElse(Collections.emptyList()).stream().map(packageNameBlock -> {
            PackageNameBlockInfo packageNameBlockInfo = PackageNameBlockInfo.builder().build();
            BeanUtils.copyProperties(packageNameBlock, packageNameBlockInfo);
            return packageNameBlockInfo;
        }).collect(Collectors.toList()));
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void savePackageNameBlock(PackageNameBlockForm packageNameBlockForm) {
        String username = UserUtils.getUsername();
        Date date = new Date();
        try {
            MutableSecurityPolicyConfiguration mutableSecurityPolicyConfiguration = configurationManagementService.getMutableConfigurationClone().getSecurityPolicyConfiguration();
            mutableSecurityPolicyConfiguration.setBlockType(BlockTypeEnum.PACKAGE_NAME.getType());
            configurationManagementService.saveOrUpdateBlock(mutableSecurityPolicyConfiguration);
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
        if (CollectionUtils.isNotEmpty(packageNameBlockForm.getPackageNameBlocks())) {
            List<PackageNameBlock> packageNameBlockList = Lists.newArrayList();
            PackageNameBlock packageNameBlock;
            for (PackageNameBlockForm item : packageNameBlockForm.getPackageNameBlocks()) {
                packageNameBlock = PackageNameBlock.builder().build();
                BeanUtils.copyProperties(item, packageNameBlock);
                packageNameBlock.setCreateBy(username);
                packageNameBlock.setCreateTime(date);
                packageNameBlock.setUpdateBy(username);
                packageNameBlock.setUpdateTime(date);
                packageNameBlockList.add(packageNameBlock);
            }
            packageNameBlockMapper.batchInsertPackageNameBlock(packageNameBlockList);
        } else if (StringUtils.isNotBlank(packageNameBlockForm.getPackageName())) {
            PackageNameBlock packageNameBlock = PackageNameBlock.builder().build();
            BeanUtils.copyProperties(packageNameBlockForm, packageNameBlock);
            packageNameBlock.setCreateBy(username);
            packageNameBlock.setCreateTime(date);
            packageNameBlock.setUpdateBy(username);
            packageNameBlock.setUpdateTime(date);
            packageNameBlockMapper.insert(packageNameBlock);
        }
        clearCache();
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updatePackageNameBlock(PackageNameBlockForm packageNameBlockForm) {
        PackageNameBlockInfo packageNameBlock = selectOnePackageNameBlock(packageNameBlockForm);
        if (Objects.nonNull(packageNameBlock)) {
            PackageNameBlock updatePackageNameBlock = PackageNameBlock.builder().build();
            updatePackageNameBlock.setUpdateBy(UserUtils.getUsername());
            updatePackageNameBlock.setUpdateTime(new Date());
            updatePackageNameBlock.setPackageName(packageNameBlockForm.getPackageName());
            updatePackageNameBlock.setConditionValue(packageNameBlockForm.getConditionValue());
            updatePackageNameBlock.setVersion(packageNameBlockForm.getVersion());
            updatePackageNameBlock.setId(packageNameBlock.getId());
            packageNameBlockMapper.updateById(updatePackageNameBlock);
            clearCache();
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void deletePackageNameBlock(PackageNameBlockForm packageNameBlockForm) {
        if (CollectionUtils.isNotEmpty(packageNameBlockForm.getPackageNames())) {
            List<PackageNameBlock> packageNameBlockList = queryPackageNameBlock(packageNameBlockForm.getPackageNames());
            if (CollectionUtils.isNotEmpty(packageNameBlockList)) {
                packageNameBlockMapper.deleteByIds(packageNameBlockList.stream().map(item -> String.valueOf(item.getId())).collect(Collectors.toSet()));
            }
        } else {
            PackageNameBlockInfo packageNameBlock = selectOnePackageNameBlock(packageNameBlockForm);
            if (Objects.nonNull(packageNameBlock)) {
                packageNameBlockMapper.deleteById(packageNameBlock.getId());
                packageNameBlockMapper.selectById(packageNameBlock.getId());
            }
        }
        clearCache();
    }

    @Override
    public PackageNameBlockInfo selectOnePackageNameBlock(PackageNameBlockForm packageNameBlockForm) {
        PackageNameBlockInfo packageNameBlockInfo = null;
        if (Objects.nonNull(packageNameBlockForm.getId())) {
            PackageNameBlock packageNameBlock = packageNameBlockMapper.selectById(packageNameBlockForm.getId());
            packageNameBlockInfo = PackageNameBlockInfo.builder().build();
            BeanUtils.copyProperties(packageNameBlock, packageNameBlockInfo);
        } else if (StringUtils.isNotBlank(packageNameBlockForm.getPackageName())) {
            List<PackageNameBlock> packageNameBlockList = packageNameBlockMapper.selectList(Wrappers.<PackageNameBlock>lambdaQuery()
                    .eq(PackageNameBlock::getPackageName, packageNameBlockForm.getPackageName())
                    .eq(StringUtils.isNotBlank(packageNameBlockForm.getConditionValue()),PackageNameBlock::getConditionValue,packageNameBlockForm.getConditionValue())
                    .eq(StringUtils.isNotBlank(packageNameBlockForm.getVersion()),PackageNameBlock::getVersion, packageNameBlockForm.getVersion())
                    .orderByDesc(PackageNameBlock::getCreateTime)
            );
            if (CollectionUtils.isNotEmpty(packageNameBlockList)) {
                PackageNameBlock packageNameBlock = packageNameBlockList.get(0);
                packageNameBlockInfo = PackageNameBlockInfo.builder().build();
                BeanUtils.copyProperties(packageNameBlock, packageNameBlockInfo);
            }
        }
        return packageNameBlockInfo;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void parseConfig() {
        final SecurityPolicyConfiguration securityPolicyConfiguration = configurationManagementService.getConfiguration().getSecurityPolicyConfiguration();
        Set<String> packageNames = securityPolicyConfiguration.getPackageNames();
        if (CollectionUtils.isEmpty(packageNames)) {
            return;
        }
        String separator = ",", name, condition, version;
        List<String> splitList;
        List<PackageNameBlock> packageNameBlockList = Lists.newArrayList();
        PackageNameBlock packageNameBlock;
        String username = UserUtils.getUsername();
        Date date = new Date();
        for (String packageName : packageNames) {
            if (StringUtils.isBlank(packageName)) {
                continue;
            }
            if (packageName.contains(separator)) {
                splitList = Arrays.asList(packageName.split(separator));
                if (splitList.size() != 3) {
                    continue;
                }
                name = splitList.get(0);
                condition = splitList.get(1);
                version = splitList.get(2);
                packageNameBlock = PackageNameBlock.builder().createBy(username).createTime(date).updateBy(username).updateTime(date)
                        .packageName(name).conditionValue(parseConditionValue(condition)).version(parseVersion(condition, version)).build();
                packageNameBlockList.add(packageNameBlock);
            } else {
                packageNameBlock = PackageNameBlock.builder().createBy(username).createTime(date).updateBy(username).updateTime(date)
                        .packageName(packageName).build();
                packageNameBlockList.add(packageNameBlock);
            }
        }
        if (CollectionUtils.isNotEmpty(packageNameBlockList)) {
            packageNameBlockMapper.batchInsertPackageNameBlock(packageNameBlockList);
        }
    }

    @Override
    public List<PackageNameBlock> getPackageNameBlockCache() {
        List<PackageNameBlock> packageNameBlockList = getCache();
        if (CollectionUtils.isEmpty(packageNameBlockList)) {
            packageNameBlockList = packageNameBlockMapper.selectList(Wrappers.<PackageNameBlock>lambdaQuery());
            putCache(packageNameBlockList, 8);
        }
        return packageNameBlockList;
    }

    /**
     * 是否存在
     *
     * @param packageNameBlockForm 参数
     * @return id
     */
    private Long exists(PackageNameBlockForm packageNameBlockForm) {
        PackageNameBlockInfo packageNameBlockInfo = selectOnePackageNameBlock(packageNameBlockForm);
        return Objects.nonNull(packageNameBlockInfo) ? packageNameBlockInfo.getId() : null;
    }

    private String parseConditionValue(String condition) {
        String conditionValue = ConditionTypeEnum.RANGE.getCondition();
        if (VersionConditionTypeEnum.EQ.getCondition().equals(condition)) {
            return ConditionTypeEnum.EQ.getCondition();
        }
        return conditionValue;
    }

    private String parseVersion(String condition, String version) {
        String versionValue = "";
        if (VersionConditionTypeEnum.EQ.getCondition().equals(condition)) {
            return version;
        }
        if (VersionConditionTypeEnum.LT.getCondition().equals(condition)) {
            return "(*," + version + ")";
        } else if (VersionConditionTypeEnum.LE.getCondition().equals(condition)) {
            return "(*," + version + "]";
        }
        return versionValue;
    }

    /**
     * 查询列表
     *
     * @param packageNames 包名
     * @return 列表
     */
    public List<PackageNameBlock> queryPackageNameBlock(List<String> packageNames) {
        return packageNameBlockMapper.selectList(Wrappers.<PackageNameBlock>lambdaQuery()
                .in(PackageNameBlock::getPackageName,packageNames)
                .orderByDesc(PackageNameBlock::getCreateTime)
        );

    }

    /**
     * 刷新缓存
     */
    private void clearCache() {
        Map<String, List<PackageNameBlock>> hazelcastMap = hazelcastInstance.getMap(PACKAGE_NAME_BLOCK_CACHE);
        hazelcastMap.remove(PACKAGE_NAME_BLOCK);
    }

    /**
     * 加入缓存
     *
     * @param cacheValue 缓存值
     * @param ttl        缓存时间，小时
     */
    private void putCache(List<PackageNameBlock> cacheValue, long ttl) {
        if (CollectionUtils.isEmpty(cacheValue)) {
            return;
        }
        hazelcastInstance.getMap(PACKAGE_NAME_BLOCK_CACHE).put(PACKAGE_NAME_BLOCK, cacheValue, ttl, TimeUnit.HOURS);
    }

    /**
     * 获取缓存
     *
     * @return 缓存值
     */
    private List<PackageNameBlock> getCache() {
        try {
            Map<String, List<PackageNameBlock>> hazelcastMap = hazelcastInstance.getMap(PACKAGE_NAME_BLOCK_CACHE);
            return hazelcastMap.get(PACKAGE_NAME_BLOCK);
        } catch (Exception ex) {
            log.warn(ExceptionUtils.getStackTrace(ex));
            return null;
        }
    }

}
