package com.veadan.folib.eventlistener.promotion;

import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.components.artifact.ArtifactComponent;
import com.veadan.folib.components.promotion.ArtifactPromotionProvider;
import com.veadan.folib.components.promotion.ArtifactPromotionProviderRegistry;
import com.veadan.folib.configuration.UnionRepositoryConfiguration;
import com.veadan.folib.configuration.UnionTargetRepositoryConfiguration;
import com.veadan.folib.controllers.federal.res.FederalPromotionPolicyRes;
import com.veadan.folib.controllers.federal.res.FederalRepositoryRes;
import com.veadan.folib.controllers.federal.res.PromotionRuleRes;
import com.veadan.folib.dispatch.ClusterDispatchNodeDto;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.policy.FederalPromotionPolicyService;
import com.veadan.folib.domain.policy.dto.SyncArtifatDTO;
import com.veadan.folib.enums.ArtifactoryRepositoryTypeEnum;
import com.veadan.folib.enums.PromotionStatusEnum;
import com.veadan.folib.enums.UnionRepositorySyncTypeEnum;
import com.veadan.folib.event.artifact.ArtifactEvent;
import com.veadan.folib.event.artifact.ArtifactEventTypeEnum;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.layout.DockerFileSystem;
import com.veadan.folib.scanner.entity.ScanRules;
import com.veadan.folib.scanner.mapper.ScanRulesMapper;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.services.JFrogService;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.ws.common.FolibWsRunManageUtil;
import com.veadan.folib.ws.common.FolibWsRunManageV2;
import com.veadan.folib.ws.server.Command;
import com.veadan.folib.ws.server.WSMessageRequest;
import com.veadan.folib.ws.server.WSMessageResponse;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;
import tk.mybatis.mapper.entity.Example;

import javax.inject.Inject;
import javax.websocket.Session;
import java.io.IOException;
import java.util.*;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * @author leipenghui
 * 事件监听，处理制品自动晋级
 **/
@Slf4j
@Component
public class ArtifactEventPromotionListener {

    @Inject
    private ArtifactPromotionProviderRegistry artifactPromotionProviderRegistry;

    @Inject
    private ArtifactComponent artifactComponent;

    @Inject
    @Lazy
    private ScanRulesMapper scanRulesMapper;

    @Inject
    private FederalPromotionPolicyService federalPromotionPolicyService;

    @Autowired
    private FolibWsRunManageV2 folibWsRunManageV2;
    @Autowired
    @Lazy
    protected ConfigurationManagementService configurationManagementService;
    @Autowired
    @Lazy
    private JFrogService jFrogService;

    //@EventListener
    public void handle(final ArtifactEvent<RepositoryPath> event) {
        try {
            int source = (int) event.getSource();
            RepositoryPath repositoryPath = event.getPath();
            ArtifactEventTypeEnum artifactEventTypeEnum = ArtifactEventTypeEnum.queryArtifactEventTypeEnumByType(source);
            log.debug("监听到制品事件 [{}]，path路径 [{}]", artifactEventTypeEnum, repositoryPath);
            if (Objects.isNull(artifactEventTypeEnum)) {
                return;
            }
            if (validateArtifactEvent(artifactEventTypeEnum) && artifactComponent.layoutSupportsForPromotion(repositoryPath)) {

                Repository repository = artifactComponent.getRepository(repositoryPath.getStorageId(), repositoryPath.getRepositoryId());
                if (Objects.isNull(repository)) {
                    log.debug("仓库 [{}] 不存在，无后续操作", RepositoryFiles.relativizePath(repositoryPath));
                    return;
                }
                Artifact artifact = repositoryPath.getArtifactEntry();
                if (Objects.isNull(artifact)) {
                    log.debug("制品 [{}] 不存在，无后续操作", RepositoryFiles.relativizePath(repositoryPath));
                    return;
                }
                String storageId = repository.getStorage().getId();
                String repositoryId = repository.getId();
                UnionRepositoryConfiguration unionRepositoryConfiguration = repository.getUnionRepositoryConfig();
                if (Objects.isNull(unionRepositoryConfiguration)) {
                    log.debug("存储空间 [{}] 仓库 [{}] 未设置联邦仓库，无后续操作", storageId, repositoryId);
                    return;
                }
                if (Boolean.FALSE.equals(unionRepositoryConfiguration.getEnable())) {
                    log.debug("存储空间 [{}] 仓库 [{}] 晋级未启用，无后续操作", storageId, repositoryId);
                    return;
                }
                log.info("存储空间 [{}] 仓库 [{}] 联邦仓库信息 [{}]", storageId, repositoryId, JSONObject.toJSONString(unionRepositoryConfiguration));
                Set<UnionTargetRepositoryConfiguration> unionTargetRepositoryConfigurations = unionRepositoryConfiguration.getUnionTargetRepositories();
                if (CollectionUtils.isEmpty(unionTargetRepositoryConfigurations)) {
                    log.debug("存储空间 [{}] 仓库 [{}] 未设置联邦目标仓库，无后续操作", storageId, repositoryId);
                    return;
                }
                String artifactPath = getArtifactPath(repositoryPath, artifact);
                Integer syncType = unionRepositoryConfiguration.getSyncType();
                boolean promotionFlag = false;
                if (UnionRepositorySyncTypeEnum.ARTIFACT_PATH.getType().equals(syncType)) {
                    //制品路径
                    Set<String> artifactPaths = unionRepositoryConfiguration.getArtifactPaths();
                    log.debug("存储空间 [{}] 仓库 [{}] 制品配置路径 [{}]，制品路径 [{}]", storageId, repositoryId, artifactPaths, artifactPath);
                    promotionFlag = artifactPaths.stream().allMatch("*"::equals) || artifactPaths.stream().anyMatch(artifactPath::contains);
                    if (!promotionFlag) {
                        //使用正则再匹配一次
                        promotionFlag = artifactPaths.stream().anyMatch(regex -> Pattern.matches(regex, artifactPath));
                    }
                } else if (UnionRepositorySyncTypeEnum.METADATA.getType().equals(syncType)) {
                    //元数据
                    JSONObject metadataJson = artifactComponent.getMetadata(artifact);
                    if (Objects.isNull(metadataJson)) {
                        log.debug("存储空间 [{}] 仓库 [{}] 制品 [{}] 未找到元数据，无后续操作", storageId, repositoryId, artifactPath);
                        return;
                    }
                    String metadataKey = unionRepositoryConfiguration.getMetadataKey();
                    String metadataValue = unionRepositoryConfiguration.getMetadataValue();
                    String valueKey = "value";
                    log.debug("存储空间 [{}] 仓库 [{}] 制品 [{}] 配置元数据key [{}] 配置元数据value [{}] 元数据 [{}]", storageId, repositoryId, artifactPath, metadataKey, metadataValue, metadataJson);
                    promotionFlag = metadataJson.containsKey(metadataKey) && metadataJson.getJSONObject(metadataKey).get(valueKey).equals(metadataValue);
                }
                if (promotionFlag) {
                    boolean promotionBlock = artifactComponent.promotionBlock();
                    Example example = new Example(ScanRules.class);
                    example.createCriteria().andEqualTo("onScan", 1).andEqualTo("storage", storageId)
                            .andEqualTo("repository", repositoryId);
                    List<ScanRules> scanRulesList = scanRulesMapper.selectByExample(example);
                    boolean scanEnable = CollectionUtils.isNotEmpty(scanRulesList);
                    log.debug("自动晋级阻断开关状态 [{}]，仓库扫描状态 [{}]", promotionBlock, scanEnable);
                    if (promotionBlock && scanEnable) {
                        //加入晋级
                        log.debug("存储空间 [{}] 仓库 [{}] 制品 [{}] 满足初步晋级条件，晋级状态为待晋级", storageId, repositoryId, artifactPath);
                        for (UnionTargetRepositoryConfiguration unionTargetRepository : unionTargetRepositoryConfigurations) {
                            artifactComponent.handlerArtifactPromotion(unionTargetRepository.getNode(), artifact.getStorageId(), artifact.getRepositoryId(), artifact.getArtifactPath(), PromotionStatusEnum.WAIT.getStatus());
                        }
                    } else {
                        //开始晋级
                        for (UnionTargetRepositoryConfiguration unionTargetRepository : unionTargetRepositoryConfigurations) {
                            try {
                                ArtifactPromotionProvider artifactPromotionProvider = artifactPromotionProviderRegistry.getProvider(unionTargetRepository.getType());
                                artifactPromotionProvider.promotion(repositoryPath, artifact.getArtifactPath(), unionTargetRepository);
                            } catch (Exception ex) {
                                log.error("存储空间 [{}] 仓库 [{}] 处理自动晋级，repositoryPath [{}] 联邦仓库 [{}] 错误 [{}]", storageId, repositoryId, repositoryPath, JSONObject.toJSONString(unionTargetRepository), ExceptionUtils.getStackTrace(ex));
                                artifactComponent.handlerArtifactPromotion(unionTargetRepository.getNode(), artifact.getStorageId(), artifact.getRepositoryId(), artifact.getArtifactPath(), PromotionStatusEnum.FAIL.getStatus());
                            }
                        }
                    }
                }
            }
        } catch (Exception ex) {
            log.error("事件监听，处理自动晋级，事件类型 [{}] repositoryPath [{}] error [{}]", event.getSource(), event.getPath(), ExceptionUtils.getStackTrace(ex));
        }
    }

    /**
     * 校验制品事件类型是否为需要处理的类型
     *
     * @param artifactEventTypeEnum 制品事件类型
     * @return true 需要处理 false 不需要处理
     */
    private boolean validateArtifactEvent(ArtifactEventTypeEnum artifactEventTypeEnum) {
        List<Integer> list = Arrays.asList(ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_STORED.getType(), ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_UPDATED.getType(), ArtifactEventTypeEnum.EVENT_ARTIFACT_METADATA_UPDATE.getType());
        return list.contains(artifactEventTypeEnum.getType());
    }

    private boolean validateArtifactDeleteEvent(ArtifactEventTypeEnum artifactEventTypeEnum) {
        List<Integer> list = Arrays.asList(ArtifactEventTypeEnum.EVENT_ARTIFACT_DIRECTORY_PATH_DELETED.getType(), ArtifactEventTypeEnum.EVENT_ARTIFACT_PATH_DELETED.getType());
        return list.contains(artifactEventTypeEnum.getType());
    }

    /**
     * 获取制品路径
     *
     * @param repositoryPath repositoryPath
     * @param artifact       artifact
     * @return 制品路径
     */
    private String getArtifactPath(RepositoryPath repositoryPath, Artifact artifact) {
        String artifactPath = artifact.getArtifactPath();
        if (repositoryPath.getFileSystem() instanceof DockerFileSystem) {
            String path = artifact.getArtifactPath();
            artifactPath = path.substring(0, path.indexOf("/sha256"));
        }
        return artifactPath;
    }

    @EventListener
    public void handlePolicy(final ArtifactEvent<RepositoryPath> event) {
        try {
            int source = (int) event.getSource();
            RepositoryPath repositoryPath = event.getPath();
            ArtifactEventTypeEnum artifactEventTypeEnum = ArtifactEventTypeEnum.queryArtifactEventTypeEnumByType(source);
            log.debug("监听到制品事件 [{}]，path路径 [{}]", artifactEventTypeEnum, repositoryPath);
            if (Objects.isNull(artifactEventTypeEnum)) {
                return;
            }
            if ((validateArtifactEvent(artifactEventTypeEnum) || validateArtifactDeleteEvent(artifactEventTypeEnum)) && artifactComponent.layoutSupportsForPromotion(repositoryPath)) {

                Repository repository = artifactComponent.getRepository(repositoryPath.getStorageId(), repositoryPath.getRepositoryId());
                if (Objects.isNull(repository)) {
                    log.debug("仓库 [{}] 不存在，无后续操作", RepositoryFiles.relativizePath(repositoryPath));
                    return;
                }

                String storageId = repository.getStorage().getId();
                String repositoryId = repository.getId();
                //UnionRepositoryConfiguration unionRepositoryConfiguration = repository.getUnionRepositoryConfig();
                List<FederalRepositoryRes> repositoryRes = federalPromotionPolicyService.queryByStorageIdAndRepositoryId(storageId, repositoryId);
                if (repositoryRes.isEmpty()) {
                    log.debug("存储空间 [{}] 仓库 [{}] 未设置联邦仓库，无后续操作", storageId, repositoryId);
                    return;
                }
                for (FederalRepositoryRes federalRepositoryRes : repositoryRes) {
                    handleFederPromotionPolicy(repositoryPath, federalRepositoryRes, artifactEventTypeEnum);
                }
            }
        } catch (Exception ex) {
            log.error("事件监听，处理自动晋级，事件类型 [{}] repositoryPath [{}] error [{}]", event.getSource(), event.getPath(), ExceptionUtils.getStackTrace(ex));
        }
    }

    public void handleFederPromotionPolicy(RepositoryPath repositoryPath, FederalRepositoryRes federalRepositoryRes, ArtifactEventTypeEnum artifactEventTypeEnum) throws IOException {
        FederalPromotionPolicyRes policyDetail = federalPromotionPolicyService.policyDetail(federalRepositoryRes.getPolicyId());
        String storageId = federalRepositoryRes.getStorageId();
        String repositoryId = federalRepositoryRes.getRepositoryId();


        if (Boolean.FALSE.equals(policyDetail.getIsEnabled())) {
            log.debug("存储空间 [{}] 仓库 [{}] 晋级未启用，无后续操作", storageId, repositoryId);
            return;
        }
        log.info("存储空间 [{}] 仓库 [{}] 联邦仓库信息 [{}]", storageId, repositoryId, JSONObject.toJSONString(policyDetail));
        //Set<UnionTargetRepositoryConfiguration> unionTargetRepositoryConfigurations = unionRepositoryConfiguration.getUnionTargetRepositories();
        List<FederalRepositoryRes> targetRepositories = policyDetail.getTargetRepositories();
        if (CollectionUtils.isEmpty(targetRepositories)) {
            log.debug("存储空间 [{}] 仓库 [{}] 未设置联邦目标仓库，无后续操作", storageId, repositoryId);
            return;
        }
        Artifact artifact = repositoryPath.getArtifactEntry();

        //联邦仓库晋级
        if(validateArtifactEvent(artifactEventTypeEnum)){
            if (Objects.isNull(artifact)) {
                log.debug("制品 [{}] 不存在，无后续操作", RepositoryFiles.relativizePath(repositoryPath));
                return;
            }
            String artifactPath = getArtifactPath(repositoryPath, artifact);

            if(policyDetail.getPathRules().isEmpty() && policyDetail.getMetadataRules().isEmpty()){
                handleFederalPromotion(repositoryPath, artifact, policyDetail);
            }else {
                boolean promotionFlag = validatePath(repositoryPath, artifactPath, policyDetail);
                boolean promotionMataDataFlag =  validateMetadata(repositoryPath, artifact, artifactPath, policyDetail);
                if (promotionFlag || promotionMataDataFlag) {
                    handleFederalPromotion(repositoryPath, artifact, policyDetail);
                }
            }

            //联邦仓库删除同步
        }else if (policyDetail.getIsDeleteSync()  && validateArtifactDeleteEvent(artifactEventTypeEnum)) {
            String artifactPath ;
            if(artifact == null){
                String path = repositoryPath.getTarget().toString();
                artifactPath = path.substring(path.lastIndexOf(String.format("%s/",repositoryId)) + repositoryId.length() + 1);
            }else {
                artifactPath = artifact.getArtifactPath();
            }
            handleFederalDeleteSync(repositoryPath,artifactPath, policyDetail);
        }

    }

    public boolean validatePath(RepositoryPath repositoryPath, String artifactPath, FederalPromotionPolicyRes policyDetail) {
        boolean promotionFlag = false;

        if (!policyDetail.getPathRules().isEmpty()) {
            String storageId = repositoryPath.getStorageId();
            String repositoryId = repositoryPath.getRepositoryId();
            //制品路径
            Set<String> pathRules = policyDetail.getPathRules().stream().map(PromotionRuleRes::getAttributeValue).collect(Collectors.toSet());
            log.debug("存储空间 [{}] 仓库 [{}] 制品配置路径 [{}]，制品路径 [{}]", storageId, repositoryId, pathRules, artifactPath);
            promotionFlag = pathRules.stream().allMatch("*"::equals) || pathRules.stream().anyMatch(artifactPath::contains);
            if (!promotionFlag) {
                //使用正则再匹配一次
                promotionFlag = pathRules.stream().anyMatch(regex -> Pattern.matches(regex, artifactPath));
            }
        }
        return promotionFlag;
    }

    public boolean validateMetadata(RepositoryPath repositoryPath, Artifact artifact, String artifactPath, FederalPromotionPolicyRes policyDetail) {
        boolean promotionMataDataFlag = false;

        if (!policyDetail.getMetadataRules().isEmpty()) {
            String storageId = repositoryPath.getStorageId();
            String repositoryId = repositoryPath.getRepositoryId();
            //元数据
            JSONObject metadataJson = artifactComponent.getMetadata(artifact);
            if (Objects.isNull(metadataJson)) {
                log.debug("存储空间 [{}] 仓库 [{}] 制品 [{}] 未找到元数据，无后续操作", storageId, repositoryId, artifactPath);
                return promotionMataDataFlag;
            }
            for (PromotionRuleRes metadataRule : policyDetail.getMetadataRules()) {
                String metadataKey = metadataRule.getAttributeKey();
                String metadataValue = metadataRule.getAttributeValue();
                String valueKey = "value";
                log.debug("晋级策略编号：{} 存储空间 [{}] 仓库 [{}] 制品 [{}] 配置元数据key [{}] 配置元数据value [{}] 元数据 [{}]", policyDetail.getPolicyId(), storageId, repositoryId, artifactPath, metadataKey, metadataValue, metadataJson);
                promotionMataDataFlag = metadataJson.containsKey(metadataKey) && metadataJson.getJSONObject(metadataKey).get(valueKey).equals(metadataValue);
                if (promotionMataDataFlag) {
                    break;
                }
            }
        }
        return promotionMataDataFlag;
    }

    public void handleFederalPromotion(RepositoryPath repositoryPath, Artifact artifact, FederalPromotionPolicyRes policyDetail) {
        boolean promotionBlock = artifactComponent.promotionBlock();
        String artifactPath = getArtifactPath(repositoryPath, artifact);
        Example example = new Example(ScanRules.class);
        example.createCriteria().andEqualTo("onScan", 1).andEqualTo("storage", repositoryPath.getStorageId())
                .andEqualTo("repository", repositoryPath.getRepositoryId());
        List<ScanRules> scanRulesList = scanRulesMapper.selectByExample(example);
        boolean scanEnable = CollectionUtils.isNotEmpty(scanRulesList);
        List<FederalRepositoryRes> targetRepositories = policyDetail.getTargetRepositories();
        log.debug("自动晋级阻断开关状态 [{}]，仓库扫描状态 [{}]", promotionBlock, scanEnable);
        if (promotionBlock && scanEnable) {
            //加入晋级
            log.debug("晋级策略编号：{} 存储空间 [{}] 仓库 [{}] 制品 [{}] 满足初步晋级条件，晋级状态为待晋级", policyDetail.getPolicyId(), repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), artifactPath);
            for (FederalRepositoryRes targetRepository : targetRepositories) {
                artifactComponent.handlerArtifactPromotion(targetRepository.getNodeName(), artifact.getStorageId(), artifact.getRepositoryId(), artifact.getArtifactPath(), PromotionStatusEnum.WAIT.getStatus());
            }
        } else {
            //开始晋级
            for (FederalRepositoryRes targetRepository : targetRepositories) {
                try {
                    ArtifactPromotionProvider artifactPromotionProvider = artifactPromotionProviderRegistry.getProvider(targetRepository.getNodeType());
                    artifactPromotionProvider.promotion(repositoryPath, artifact.getArtifactPath(), targetRepository);
                } catch (Exception ex) {
                    log.error("晋级策略编号：{} 存储空间 [{}] 仓库 [{}] 处理自动晋级，repositoryPath [{}] 联邦仓库 [{}] 错误 [{}]", policyDetail.getPolicyId(), repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), repositoryPath, JSONObject.toJSONString(targetRepository), ExceptionUtils.getStackTrace(ex));
                    artifactComponent.handlerArtifactPromotion(targetRepository.getNodeName(), artifact.getStorageId(), artifact.getRepositoryId(), artifact.getArtifactPath(), PromotionStatusEnum.FAIL.getStatus());
                }
            }
        }
    }

    public void handleFederalDeleteSync(RepositoryPath repositoryPath ,String artifactPath, FederalPromotionPolicyRes policyDetail) {
        Map<String, ClusterDispatchNodeDto> map = configurationManagementService.
                getMutableConfigurationClone().getClusterDispatchNode();
        if (MapUtils.isEmpty(map)) {
            return;
        }
        FederalRepositoryRes federalRepositoryRes = policyDetail.getTargetRepositories().get(0);
        if(ArtifactoryRepositoryTypeEnum.JFROG.getType().equals(federalRepositoryRes.getNodeType())) {
            for (FederalRepositoryRes targetRepository : policyDetail.getTargetRepositories()){
                jFrogService.deletePat(targetRepository.getNodeName(),targetRepository.getRepositoryId(),artifactPath);
            }
            return;
        }

        ClusterDispatchNodeDto nodeDto = map.get(federalRepositoryRes.getNodeName());
        String targetHostName = FolibWsRunManageUtil.getSimpleTargetHostName(nodeDto);
        Session session = folibWsRunManageV2.getSession(targetHostName);
        nodeDto.setWsClientOnline(session != null && session.isOpen());


        WSMessageRequest wsMessageRequest = null;
        WSMessageResponse messageResponse = null;


        log.info("联邦仓库策略同步删除 晋级策略编号：{} 存储空间 [{}] 仓库 [{}]",policyDetail.getPolicyId(), repositoryPath.getStorageId(), repositoryPath.getRepositoryId());
        List<SyncArtifatDTO> artifatDTOList = policyDetail.getTargetRepositories().stream()
                .map(item -> new SyncArtifatDTO(item.getPolicyId(),
                        item.getStorageId(),
                        item.getRepositoryId(),
                        artifactPath)
                ).collect(Collectors.toList());

        try {
            //查询请求参数
            wsMessageRequest = new WSMessageRequest(Command.FEDERAL_DELETE_SYNC, artifatDTOList);
            messageResponse = folibWsRunManageV2.sendRequest(targetHostName, wsMessageRequest);
            log.info("联邦仓库策略同步删除 发送请求,请求参数:{}", JSONObject.toJSONString(artifatDTOList));
            log.debug("联邦仓库策略同步删除  result,wsMessageRequest:{},messageResponse:{}", wsMessageRequest, messageResponse);
        } catch (Exception e) {
            log.error("联邦仓库策略同步删除 fail,wsMessageRequest:{}", wsMessageRequest, e);
        }
    }
}
