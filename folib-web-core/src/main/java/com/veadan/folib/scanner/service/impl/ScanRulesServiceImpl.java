package com.veadan.folib.scanner.service.impl;

import com.google.common.collect.Lists;
import com.veadan.folib.components.DistributedCacheComponent;
import com.veadan.folib.components.thirdparty.foeyes.FoEyesComponent;
import com.veadan.folib.components.thirdparty.foeyes.enums.ClassifierEnum;
import com.veadan.folib.components.thirdparty.foeyes.reponse.ProjectInfo;
import com.veadan.folib.components.thirdparty.foeyes.request.CreateProjectRequest;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.domain.Tree;
import com.veadan.folib.scanner.biz.ScanRulesBiz;
import com.veadan.folib.scanner.entity.ScanRules;
import com.veadan.folib.scanner.service.ScanRulesService;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import tk.mybatis.mapper.entity.Example;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * @author leipenghui
 * @date 2024/4/22
 **/
@Service
public class ScanRulesServiceImpl implements ScanRulesService {

    @Autowired
    private ScanRulesBiz scanRulesBiz;

    @Autowired
    private FoEyesComponent foEyesComponent;

    @Autowired
    private ConfigurationManagementService configurationManagementService;

    @Autowired
    private DistributedCacheComponent distributedCacheComponent;

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void saveOrUpdateScanRules(ScanRules scanRules) {
        ScanRules dbScanRules = scanRulesBiz.selectById(scanRules.getId());
        if (Objects.nonNull(dbScanRules)) {
            scanRulesBiz.updateSelectiveById(scanRules);
        } else {
            scanRulesBiz.insertSelective(scanRules);
        }
        if (Boolean.TRUE.equals(scanRules.getBomOnScan()) && foEyesComponent.enable()) {
            //调用foeyes创建父项目
            CreateProjectRequest createProjectRequest = CreateProjectRequest.builder().name(String.format("%s/%s", scanRules.getStorage(), scanRules.getRepository())).classifier(ClassifierEnum.LIBRARY.getType()).build();
            ProjectInfo projectInfo = foEyesComponent.createProject(createProjectRequest);
            scanRules.setProjectUuid(projectInfo.getUuid());
            scanRulesBiz.updateSelectiveById(scanRules);
        }
        String cacheKey = String.format(GlobalConstants.SCAN_ENABLE_REPOSITORY_KEY, scanRules.getId().toUpperCase());
        distributedCacheComponent.delete(cacheKey);
    }

    @Override
    public List<ScanRules> queryBomOnScanList() {
        Example example = new Example(ScanRules.class);
        example.createCriteria().andEqualTo("bomOnScan", 1);
        return scanRulesBiz.selectByExample(example);
    }

    @Override
    public List<Tree> queryBomOnScanTree() {
        List<ScanRules> scanRulesList = queryBomOnScanList();
        if (CollectionUtils.isEmpty(scanRulesList)) {
            return null;
        }
        List<Tree> treeList = Lists.newArrayList();
        Map<String, List<ScanRules>> storageMap = scanRulesList.stream().collect(Collectors.groupingBy(ScanRules::getStorage));
        for (Map.Entry<String, List<ScanRules>> entry : storageMap.entrySet()) {
            treeList.add(Tree.builder().label(entry.getKey()).value(entry.getKey()).children(entry.getValue().stream().map(item -> Tree.builder().label(item.getRepository()).value(item.getProjectUuid()).build()).collect(Collectors.toList())).build());
        }
        return treeList;
    }

    @Override
    public List<ScanRules> queryOnScanList() {
        Example example = new Example(ScanRules.class);
        example.createCriteria().andEqualTo("onScan", 1);
        return scanRulesBiz.selectByExample(example);
    }

    @Override
    public List<Tree> queryOnScanTree() {
        final List<Storage> storageList = new ArrayList<>(configurationManagementService.getConfiguration()
                .getStorages()
                .values());
        if (CollectionUtils.isEmpty(storageList)) {
            return null;
        }
        List<Tree> treeList = Lists.newArrayList();
        Tree tree;
        if (CollectionUtils.isNotEmpty(storageList)) {
            for (Storage storage : storageList) {
                tree = Tree.builder().label(storage.getId()).value(storage.getId()).children(Lists.newArrayList()).build();
                if (MapUtils.isNotEmpty(storage.getRepositories())) {
                    for (Repository repository : storage.getRepositories().values()) {
                        tree.getChildren().add(Tree.builder().label(repository.getId()).value(repository.getId()).build());
                    }
                }
                treeList.add(tree);
            }
        }
        return treeList;
    }
}
