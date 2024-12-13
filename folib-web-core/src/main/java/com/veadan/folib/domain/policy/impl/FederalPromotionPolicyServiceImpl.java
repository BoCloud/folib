package com.veadan.folib.domain.policy.impl;

import com.veadan.folib.configuration.MutableConfiguration;
import com.veadan.folib.configuration.MutableUnionTargetRepositoryConfiguration;
import com.veadan.folib.configuration.UnionRepositoryConfiguration;
import com.veadan.folib.controllers.federal.req.*;
import com.veadan.folib.controllers.federal.res.FederalPromotionPolicyRes;
import com.veadan.folib.controllers.federal.res.FederalRepositoryRes;
import com.veadan.folib.controllers.federal.res.PromotionRuleRes;
import com.veadan.folib.domain.policy.FederalPromotionPolicyService;
import com.veadan.folib.domain.policy.dto.SyncArtifatDTO;
import com.veadan.folib.entity.FederalPromotionPolicy;
import com.veadan.folib.entity.FederalRepository;
import com.veadan.folib.entity.PromotionRule;
import com.veadan.folib.enums.TagEnum;
import com.veadan.folib.mapper.FederalPromotionPolicyMapper;
import com.veadan.folib.mapper.FederalRepositoryMapper;
import com.veadan.folib.mapper.PromotionRuleMapper;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.storage.ArtifactStorageException;
import com.veadan.folib.storage.StorageDto;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryDto;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.*;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;

import javax.annotation.PostConstruct;
import javax.annotation.Resource;
import javax.inject.Inject;
import java.io.IOException;
import java.nio.file.Files;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import static org.springframework.http.HttpStatus.NOT_FOUND;
@Slf4j
@Service
public class FederalPromotionPolicyServiceImpl implements FederalPromotionPolicyService {

    @Resource
    private FederalPromotionPolicyMapper federalPromotionPolicyMapper;
    @Resource
    private FederalRepositoryMapper federalRepositoryMapper;
    @Resource
    private PromotionRuleMapper promotionRuleMapper;
    @Resource
    private ConfigurationManagementService configurationManagementService;
    @Inject
    protected RepositoryPathResolver repositoryPathResolver;
    @Inject
    protected ArtifactManagementService artifactManagementService;

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void addPolicy(FederalPromotionPolicyCreateReq createReq) {

        // 1. 创建策略

        List<FederalPromotionPolicy> list = federalPromotionPolicyMapper.queryByName(createReq.getName());
        if (list != null && !list.isEmpty()) {
            throw new RuntimeException("策略名称重复");
        }
        FederalPromotionPolicy entity = toFederalPromotionPolicyEntity.apply(createReq);
        federalPromotionPolicyMapper.insert(entity);

        // 2. 创建源仓库
        List<FederalRepositoryCreateReq> createReqList = new ArrayList<>();
        if (createReq.getSourceRepositories() != null && !createReq.getSourceRepositories().isEmpty()) {
            createReqList.addAll(createReq.getSourceRepositories());
        }

        if (createReq.getTargetRepositories() != null && !createReq.getTargetRepositories().isEmpty()) {
            createReqList.addAll(createReq.getTargetRepositories());
        }

        List<FederalRepository> repositories = createReqList.stream().map(data -> {
            FederalRepository federalRepository = toFederalRepositoryEntity.apply(data);
            if (federalRepository != null) {
                federalRepository.setPolicyId(entity.getPolicyId());
            }
            return federalRepository;
        }).filter(Objects::nonNull).collect(Collectors.toList());
        if (!repositories.isEmpty()) {
            federalRepositoryMapper.insertBatch(repositories);
        }

        // 3. 创建规则
        List<PromotionRuleCreateReq> ruleReqs = new ArrayList<>();
        if (createReq.getPathRules() != null && !createReq.getPathRules().isEmpty()) {
            ruleReqs.addAll(createReq.getPathRules());
        }
        if (createReq.getMetadataRules() != null && !createReq.getMetadataRules().isEmpty()) {
            ruleReqs.addAll(createReq.getMetadataRules());
        }

        List<PromotionRule> rules = ruleReqs.stream().map(data -> {
            PromotionRule rule = toPromotionRuleEntity.apply(data);
            if (rule != null) {
                rule.setPolicyId(entity.getPolicyId());
            }
            return rule;
        }).filter(Objects::nonNull).collect(Collectors.toList());
        if (!rules.isEmpty()) {
            promotionRuleMapper.insertBatch(rules);
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void deletePolicy(long policyId) {
        promotionRuleMapper.deleteByPolicyId(policyId);
        federalRepositoryMapper.deleteByPolicyId(policyId);
        federalPromotionPolicyMapper.deleteById(policyId);
    }


    @Override
    @Transactional(rollbackFor = Exception.class)
    public void editPolicy(FederalPromotionPolicyUpdateReq updateReq) {

        // 1. 更新策略
        List<FederalPromotionPolicy> list = federalPromotionPolicyMapper.queryByName(updateReq.getName());
        if (list != null && !list.isEmpty() && list.stream().anyMatch(data -> data.getPolicyId() != updateReq.getPolicyId())) {
            throw new RuntimeException("策略名称重复");
        }
        FederalPromotionPolicy entity = toFederalPromotionPolicyEntityUpdate.apply(updateReq);
        federalPromotionPolicyMapper.update(entity);

        //2.删除仓库
        federalRepositoryMapper.deleteByPolicyId(updateReq.getPolicyId());
        List<FederalRepositoryUpdateReq> updateReqList = new ArrayList<>();
        if (updateReq.getSourceRepositories() != null && !updateReq.getSourceRepositories().isEmpty()) {
            updateReqList.addAll(updateReq.getSourceRepositories());
        }
        if (updateReq.getTargetRepositories() != null && !updateReq.getTargetRepositories().isEmpty()) {
            updateReqList.addAll(updateReq.getTargetRepositories());
        }
        List<FederalRepository> repositories = updateReqList.stream().map(toFederalRepositoryEntityUpdate).filter(Objects::nonNull).collect(Collectors.toList());

        if (!repositories.isEmpty()) {
            federalRepositoryMapper.insertBatch(repositories);
        }
        //3. 删除规则
        promotionRuleMapper.deleteByPolicyId(updateReq.getPolicyId());
        List<PromotionRuleUpdateReq> ruleReqs = new ArrayList<>();
        if (updateReq.getPathRules() != null && !updateReq.getPathRules().isEmpty()) {
            ruleReqs.addAll(updateReq.getPathRules());
        }

        if (updateReq.getMetadataRules() != null && !updateReq.getMetadataRules().isEmpty()) {
            ruleReqs.addAll(updateReq.getMetadataRules());
        }

        List<PromotionRule> rules = ruleReqs.stream().filter(item -> {
            return item.getAttributeValue() != null && !item.getAttributeValue().isEmpty();
        }).map(toPromotionRuleEntityUpdate).filter(Objects::nonNull).collect(Collectors.toList());
        if (!rules.isEmpty()) {
            promotionRuleMapper.insertBatch(rules);
        }
    }

    @Override
    public Page<FederalPromotionPolicyRes> paginQuery(FederalPromotionPolicyQueryReq queryReq) {
        FederalPromotionPolicy federalPromotionPolicy = toFederalPromotionPolicyEntityQuery.apply(queryReq);
        long total = federalPromotionPolicyMapper.count(federalPromotionPolicy);
        PageRequest pageRequest = PageRequest.of(queryReq.getPageNumber() - 1, queryReq.getPageSize());
        List<FederalPromotionPolicyRes> list = federalPromotionPolicyMapper.queryAllByLimit(federalPromotionPolicy, pageRequest).stream().map(toFederalPromotionPolicyRes).filter(Objects::nonNull).collect(Collectors.toList());

        for (FederalPromotionPolicyRes entity : list) {
            List<FederalRepositoryRes> repositories = federalRepositoryMapper.queryByPolicyId(entity.getPolicyId()).stream().map(toFederalRepositoryRes).collect(Collectors.toList());
            if (!repositories.isEmpty()) {
                entity.setSourceRepositories(repositories.stream().filter(data -> data.getType().equals("source")).collect(Collectors.toList()));
                entity.setTargetRepositories(repositories.stream().filter(data -> data.getType().equals("target")).collect(Collectors.toList()));
            }
            List<PromotionRuleRes> rules = promotionRuleMapper.queryByPolicyId(entity.getPolicyId()).stream().map(toPromotionRuleRes).collect(Collectors.toList());
            if (!rules.isEmpty()) {
                entity.setPathRules(rules.stream().filter(data -> data.getRuleType().equals("path")).collect(Collectors.toList()));
                entity.setMetadataRules(rules.stream().filter(data -> data.getRuleType().equals("metadata")).collect(Collectors.toList()));
            }
        }
        return new PageImpl<>(list, pageRequest, total);
    }


    @Override
    public FederalPromotionPolicyRes policyDetail(long policyId) {

        FederalPromotionPolicy policy = federalPromotionPolicyMapper.queryById(policyId);
        if (policy == null) {
            return null;
        }

        FederalPromotionPolicyRes res = toFederalPromotionPolicyRes.apply(policy);
        List<FederalRepositoryRes> repositoryResList = federalRepositoryMapper.queryByPolicyId(policyId).stream().map(toFederalRepositoryRes).collect(Collectors.toList());
        List<PromotionRuleRes> ruleResList = promotionRuleMapper.queryByPolicyId(policyId).stream().map(toPromotionRuleRes).collect(Collectors.toList());

        for (FederalRepositoryRes repositoryRes : repositoryResList) {
            if (repositoryRes.getType().equals("source")) {
                res.getSourceRepositories().add(repositoryRes);
            }
            if (repositoryRes.getType().equals("target")) {
                res.getTargetRepositories().add(repositoryRes);
            }
        }
        for (PromotionRuleRes ruleRes : ruleResList) {
            if (ruleRes.getRuleType().equals("path")) {
                res.getPathRules().add(ruleRes);
            }
            if (ruleRes.getRuleType().equals("metadata")) {
                res.getMetadataRules().add(ruleRes);
            }
        }
        return res;
    }

    /**
     * 根据存储空间id和仓库id查询
     *
     * @param storageId    存储空间ID
     * @param repositoryId 仓库ID
     * @return FederalPromotionPolicyRes
     */
    @Override
    public List<FederalRepositoryRes> queryByStorageIdAndRepositoryId(String storageId, String repositoryId) {
        return federalRepositoryMapper.queryByStorageIdAndRepositoryId(storageId, repositoryId, "source").stream().map(data -> toFederalRepositoryRes.apply(data)).collect(Collectors.toList());
    }

    /**
     * 联邦制品删除同步
     *
     * @param syncArtifatDTOS 同步参数
     */
    @Override
    public void federalDeleteArtifatSync(List<SyncArtifatDTO> syncArtifatDTOS) {
        if (syncArtifatDTOS== null || syncArtifatDTOS.isEmpty()) {
            return;
        }
        for (SyncArtifatDTO syncArtifatDTO : syncArtifatDTOS) {
            final String storageId = syncArtifatDTO.getStorageId();
            final String repositoryId = syncArtifatDTO.getRepositoryId();
            final String artifactPath = syncArtifatDTO.getArtifactPath();
            log.info("federalDeleteArtifatSync 联邦策略ID：{} path {}:{}/{}...",syncArtifatDTO.getPolicyId(), storageId, repositoryId, artifactPath);
            try {
                final RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
                if (!Files.exists(repositoryPath)) {
                    break;
                }
                artifactManagementService.delete(repositoryPath, false);
            } catch (IOException e) {
                log.error("federalDeleteArtifatSync 删除失败：联邦策略ID：{} path {}:{}/{}...",syncArtifatDTO.getPolicyId(), storageId, repositoryId, artifactPath);
                log.error(e.getMessage(), e);
            }
        }
    }

    @PostConstruct
    public void initData() {
        FederalPromotionPolicy federalPromotionPolicy = new FederalPromotionPolicy();
        federalPromotionPolicy.setTag(TagEnum.DEFAULT.toString());
        long total = federalPromotionPolicyMapper.count(federalPromotionPolicy);
        if (total == 0) {
            MutableConfiguration storage = configurationManagementService.getMutableConfigurationClone();
            Map<String, StorageDto> dtoMap = storage.getStorages();
            for (String key : dtoMap.keySet()) {
                List<RepositoryDto> repositoryList = dtoMap.get(key).getRepositories().values().stream()
                        .filter(data -> data instanceof RepositoryDto)
                        .map(data -> (RepositoryDto) data)
                        .filter(data -> data.getUnionRepositoryConfiguration() != null)
                        .collect(Collectors.toList());
                handleOldData(key, repositoryList);
            }
        }
    }

    public void restOldData() {
        FederalPromotionPolicy federalPromotionPolicy = new FederalPromotionPolicy();
        federalPromotionPolicy.setTag(TagEnum.DEFAULT.toString());
        long total = federalPromotionPolicyMapper.count(federalPromotionPolicy);
        PageRequest pageRequest = PageRequest.of(0, Integer.parseInt(String.valueOf(total)));
        List<FederalPromotionPolicy> policies = federalPromotionPolicyMapper.queryAllByLimit(federalPromotionPolicy, pageRequest);
        if (!policies.isEmpty()) {
            for (FederalPromotionPolicy policy : policies) {
                deletePolicy(policy.getPolicyId());
            }
        }
        MutableConfiguration storage = configurationManagementService.getMutableConfigurationClone();
        Map<String, StorageDto> dtoMap = storage.getStorages();
        for (String key : dtoMap.keySet()) {
            List<RepositoryDto> repositoryList = dtoMap.get(key).getRepositories().values().stream()
                    .filter(data -> data instanceof RepositoryDto)
                    .map(data -> (RepositoryDto) data)
                    .filter(data -> data.getUnionRepositoryConfiguration() != null)
                    .collect(Collectors.toList());
            handleOldData(key, repositoryList);
        }
    }

    public void handleOldData(String storageId, List<RepositoryDto> repositories) {

        if (repositories.isEmpty()) {
            return;
        }
        for (RepositoryDto repository : repositories) {
            handleFailureOldData(storageId, repository);

        }
    }

    public void handleFailureOldData(String storageId, RepositoryDto repository) {
        FederalPromotionPolicy entity = new FederalPromotionPolicy();
        entity.setName(String.format("federal-%s-%s", storageId, repository.getId()));
        entity.setIsEnabled(repository.getUnionRepositoryConfiguration().getEnable());
        entity.setCreatedBy("system");
        entity.setCreatedTime(new Date());
        entity.setTag(TagEnum.DEFAULT.toString());
        federalPromotionPolicyMapper.insert(entity);
        List<FederalRepository> repositories = new ArrayList<>();

        FederalRepository sourceRepository = new FederalRepository();
        sourceRepository.setPolicyId(entity.getPolicyId());
        sourceRepository.setType("source");
        sourceRepository.setStorageId(storageId);
        sourceRepository.setRepositoryId(repository.getId());
        sourceRepository.setCreatedTime(new Date());
        repositories.add(sourceRepository);
        for (MutableUnionTargetRepositoryConfiguration data : repository.getUnionRepositoryConfiguration().getUnionTargetRepositories()) {
            FederalRepository targetRepository = new FederalRepository();
            targetRepository.setPolicyId(entity.getPolicyId());
            targetRepository.setType("target");
            targetRepository.setStorageId(data.getStorageId());
            targetRepository.setRepositoryId(data.getRepositoryId());
            targetRepository.setNodeName(data.getNode());
            targetRepository.setNodeType(data.getType());
            targetRepository.setCreatedTime(new Date());
            repositories.add(targetRepository);
        }
        federalRepositoryMapper.insertBatch(repositories);

        List<PromotionRule> rules = new ArrayList<>();
        List<String> paths = new ArrayList<>(repository.getUnionRepositoryConfiguration().getArtifactPaths());
        if (!paths.isEmpty()) {
            for (String path : paths) {
                PromotionRule pathRule = new PromotionRule();
                pathRule.setPolicyId(entity.getPolicyId());
                pathRule.setRuleType("path");
                pathRule.setAttributeValue(path);
                pathRule.setCreatedTime(new Date());
                rules.add(pathRule);
            }

        }
        if (repository.getUnionRepositoryConfiguration().getMetadataKey() != null) {
            PromotionRule metadataRule = new PromotionRule();
            metadataRule.setPolicyId(entity.getPolicyId());
            metadataRule.setRuleType("metadata");
            metadataRule.setAttributeKey(repository.getUnionRepositoryConfiguration().getMetadataKey());
            metadataRule.setAttributeValue(repository.getUnionRepositoryConfiguration().getMetadataValue());
            metadataRule.setCreatedTime(new Date());
            rules.add(metadataRule);
        }

        if (!rules.isEmpty()) {
            promotionRuleMapper.insertBatch(rules);
        }
    }


    Function<FederalPromotionPolicyCreateReq, FederalPromotionPolicy> toFederalPromotionPolicyEntity = (req) -> {
        if (req == null) {
            return null;
        }
        FederalPromotionPolicy policy = new FederalPromotionPolicy();
        policy.setName(req.getName());
        policy.setIsEnabled(req.getIsEnabled());
        policy.setTag(req.getTag());
        policy.setIsDeleteSync(req.getIsDeleteSync());
        policy.setCreatedTime(new Date());
        policy.setCreatedBy(req.getCreatedBy());
        return policy;
    };

    Function<FederalRepositoryCreateReq, FederalRepository> toFederalRepositoryEntity = (req) -> {
        if (req == null) {
            return null;
        }
        FederalRepository repository = new FederalRepository();
        repository.setType(req.getType());
        repository.setStorageId(req.getStorageId());
        repository.setRepositoryId(req.getRepositoryId());
        repository.setNodeName(req.getNodeName());
        repository.setNodeType(req.getNodeType());
        repository.setCreatedTime(new Date());
        return repository;
    };

    Function<PromotionRuleCreateReq, PromotionRule> toPromotionRuleEntity = (req) -> {
        if (req == null) {
            return null;
        }
        PromotionRule rule = new PromotionRule();
        rule.setRuleType(req.getRuleType());
        rule.setAttributeKey(req.getAttributeKey());
        rule.setAttributeValue(req.getAttributeValue());
        rule.setCreatedTime(new Date());
        return rule;
    };

    Function<FederalPromotionPolicy, FederalPromotionPolicyRes> toFederalPromotionPolicyRes = (entity) -> {
        if (entity == null) {
            return null;
        }
        return new FederalPromotionPolicyRes().setPolicyId(entity.getPolicyId())
                .setName(entity.getName())
                .setIsEnabled(entity.getIsEnabled())
                .setIsDeleteSync(entity.getIsDeleteSync())
                .setTag(entity.getTag())
                .setCreatedTime(entity.getCreatedTime())
                .setUpdateTime(entity.getUpdateTime())
                .setCreatedBy(entity.getCreatedBy())
                .setUpdatedBy(entity.getUpdatedBy());
    };

    Function<FederalRepository, FederalRepositoryRes> toFederalRepositoryRes = (entity) -> {
        if (entity == null) {
            return null;
        }
        return new FederalRepositoryRes().setId(entity.getId())
                .setPolicyId(entity.getPolicyId())
                .setType(entity.getType())
                .setStorageId(entity.getStorageId())
                .setRepositoryId(entity.getRepositoryId())
                .setNodeName(entity.getNodeName())
                .setNodeType(entity.getNodeType())
                .setCreatedTime(entity.getCreatedTime())
                .setUpdateTime(entity.getUpdateTime());
    };

    Function<PromotionRule, PromotionRuleRes> toPromotionRuleRes = (entity) -> {
        if (entity == null) {
            return null;
        }
        return new PromotionRuleRes().setRuleId(entity.getRuleId())
                .setPolicyId(entity.getPolicyId())
                .setRuleType(entity.getRuleType())
                .setAttributeKey(entity.getAttributeKey())
                .setAttributeValue(entity.getAttributeValue())
                .setUpdateTime(entity.getUpdateTime())
                .setCreatedTime(entity.getCreatedTime());
    };


    Function<FederalPromotionPolicyUpdateReq, FederalPromotionPolicy> toFederalPromotionPolicyEntityUpdate = (req) -> {
        if (req == null) {
            return null;
        }
        return FederalPromotionPolicy.builder()
                .policyId(req.getPolicyId())
                .name(req.getName())
                .isEnabled(req.getIsEnabled())
                .isDeleteSync(req.getIsDeleteSync())
                .tag(req.getTag())
                .updateTime(new Date())
                .updatedBy(req.getUpdatedBy())
                .build();
    };

    Function<FederalRepositoryUpdateReq, FederalRepository> toFederalRepositoryEntityUpdate = (req) -> {
        if (req == null) {
            return null;
        }
        return FederalRepository.builder()
                .id(req.getId())
                .policyId(req.getPolicyId())
                .type(req.getType())
                .storageId(req.getStorageId())
                .repositoryId(req.getRepositoryId())
                .nodeName(req.getNodeName())
                .nodeType(req.getNodeType())
                .updateTime(new Date())
                .build();
    };

    Function<PromotionRuleUpdateReq, PromotionRule> toPromotionRuleEntityUpdate = (req) -> {
        if (req == null) {
            return null;
        }
        return PromotionRule.builder()
                .ruleId(req.getRuleId())
                .policyId(req.getPolicyId())
                .ruleType(req.getRuleType())
                .attributeKey(req.getAttributeKey())
                .attributeValue(req.getAttributeValue())
                .updateTime(new Date())
                .build();
    };

    Function<FederalPromotionPolicyQueryReq, FederalPromotionPolicy> toFederalPromotionPolicyEntityQuery = (req) -> {
        if (req == null) {
            return null;
        }
        return FederalPromotionPolicy.builder()
                .name(req.getName())
                .isEnabled(req.getIsEnabled())
                .tag(req.getTag())
                .build();
    };

}
