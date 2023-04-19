package com.veadan.folib.task;


import com.alibaba.fastjson.JSONObject;
import com.google.common.collect.Lists;
import com.veadan.folib.components.artifact.ArtifactComponent;
import com.veadan.folib.configuration.UnionTargetRepositoryConfiguration;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.ArtifactDispatch;
import com.veadan.folib.dto.TargetDispatchRepositoryDto;
import com.veadan.folib.enums.PromotionStatusEnum;
import com.veadan.folib.enums.SafeLevelEnum;
import com.veadan.folib.promotion.PromotionUtil;
import com.veadan.folib.providers.layout.DockerLayoutProvider;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.services.FolibDistributedSchedulerLock;
import com.veadan.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * @author leipenghui
 * 自动晋级task
 */
@Slf4j
@Component
@EnableScheduling
public class PromotionTask {

    @Inject
    @Lazy
    private ArtifactRepository artifactRepository;

    @Inject
    @Lazy
    private FolibDistributedSchedulerLock folibDistributedSchedulerLock;

    @Inject
    @Lazy
    private ArtifactComponent artifactComponent;

    @Inject
    @Lazy
    private PromotionUtil promotionUtil;

    @Scheduled(cron = "0 0/6 * * * ? ")
    public void run() {
        String lockName = "folib.PromotionTask";
        Long lockTime = 300L;
        log.info("Wait for the lock [{}]", lockName);
        if (folibDistributedSchedulerLock.getLock(lockName, lockTime)) {
            log.info("Get lock [{}]", lockName);
            //无需扫描、扫描完成
            List<String> safeLevelList = Lists.newArrayList(SafeLevelEnum.UNWANTED_SCAN.getLevel(), SafeLevelEnum.SCAN_COMPLETE.getLevel());
            //晋级失败
            List<String> promotionStatusList = Lists.newArrayList(PromotionStatusEnum.FAIL.getStatus());
            boolean promotionBlock = artifactComponent.promotionBlock();
            if (promotionBlock) {
                //晋级阻断开启，等待晋级也需要晋级
                promotionStatusList.add(PromotionStatusEnum.WAIT.getStatus());
            }
            List<Artifact> artifactList = artifactRepository.findPromotionMatchingByIndex(safeLevelList, promotionStatusList);
            if (CollectionUtils.isNotEmpty(artifactList)) {
                boolean block;
                ArtifactDispatch artifactDispatch;
                TargetDispatchRepositoryDto targetDispatchRepository;
                String storageId, repositoryId, artifactPath = "";
                Set<UnionTargetRepositoryConfiguration> unionTargetRepositories;
                UnionTargetRepositoryConfiguration unionTargetRepository = null;
                String nodePromotionStatus;
                for (Artifact artifact : artifactList) {
                    storageId = artifact.getStorageId();
                    repositoryId = artifact.getRepositoryId();
                    block = artifactComponent.vulnerabilityBlock(artifact);
                    if (block) {
                        log.info("存储空间 [{}] 所属仓库 [{}] 制品 [{}] 存在漏洞，满足安全策略配置中的阻断条件，取消晋级", storageId, repositoryId, artifact.getArtifactPath());
                        artifactComponent.handlerArtifactPromotion("", artifact, PromotionStatusEnum.BLOCK.getStatus());
                        continue;
                    }
                    //开始晋级
                    unionTargetRepositories = artifactComponent.getUnionTargetRepositories(storageId, repositoryId);
                    if (CollectionUtils.isNotEmpty(unionTargetRepositories)) {
                        Map<String, UnionTargetRepositoryConfiguration> unionTargetRepositoryConfigurationMap = Optional.ofNullable(unionTargetRepositories).orElse(Collections.emptySet()).stream().collect(Collectors.toMap(UnionTargetRepositoryConfiguration::getNode, Function.identity()));
                        //成功的节点跳过 其余的继续晋级
                        for (String node : artifact.getPromotionNodes()) {
                            try {
                                node = node.split(",")[0];
                                unionTargetRepository = unionTargetRepositoryConfigurationMap.get(node);
                                if (Objects.isNull(unionTargetRepository)) {
                                    artifactComponent.deleteArtifactPromotionNode(artifact, node);
                                    log.info("存储空间：{} 仓库：{} 处理自动晋级，制品：{} 联邦仓库：{} 不存在，移除节点", storageId, repositoryId, artifactPath, node);
                                    continue;
                                }
                                nodePromotionStatus = getNodePromotionStatus(artifact.getPromotionNodes(), unionTargetRepository.getNode());
                                if (PromotionStatusEnum.SUCCESS.getStatus().equals(nodePromotionStatus)) {
                                    //当前节点已成功晋级，继续下一个节点
                                    continue;
                                }
                                artifactPath = getArtifactPath(artifact);
                                targetDispatchRepository = TargetDispatchRepositoryDto.builder().dispatchClusterEnName(unionTargetRepository.getNode()).targetStorageId(unionTargetRepository.getStorageId()).targetRepositoryId(unionTargetRepository.getRepositoryId()).build();
                                artifactDispatch = ArtifactDispatch.builder().srcStorageId(storageId).srcRepositoryId(repositoryId).path(artifactPath)
                                        .targetDispatchRepositoryList(Collections.singletonList(targetDispatchRepository)).recordStatus(true).build();
                                promotionUtil.executeHandleDispatch(artifactDispatch);
                            } catch (Exception ex) {
                                log.error("存储空间：{} 仓库：{} 处理自动晋级，制品：{} 联邦仓库：{} 错误：{}", storageId, repositoryId, artifactPath, JSONObject.toJSONString(unionTargetRepository), ExceptionUtils.getStackTrace(ex));
                                artifactComponent.handlerArtifactPromotion(node, artifact, PromotionStatusEnum.FAIL.getStatus());
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * 获取节点的晋级状态
     *
     * @param promotionNodes promotionNodes
     * @param node           node
     * @return 晋级状态
     */
    private String getNodePromotionStatus(Set<String> promotionNodes, String node) {
        String status = "";
        List<String> list = null;
        if (CollectionUtils.isNotEmpty(promotionNodes)) {
            for (String promotionNode : promotionNodes) {
                if (promotionNode.contains(node)) {
                    list = Arrays.asList(promotionNode.split(","));
                    if (CollectionUtils.isNotEmpty(list) && list.size() > 1) {
                        status = list.get(1);
                    }
                }
            }
        }
        return status;
    }

    /**
     * 获取制品路径
     *
     * @param artifact artifact
     * @return 制品路径
     */
    private String getArtifactPath(Artifact artifact) {
        Repository repository = artifactComponent.getRepository(artifact.getStorageId(), artifact.getRepositoryId());
        String artifactPath = artifact.getArtifactPath();
        if (Objects.nonNull(repository) && DockerLayoutProvider.ALIAS.equals(repository.getLayout())) {
            String path = artifact.getArtifactPath();
            artifactPath = path.substring(0, path.indexOf("/sha256"));
        }
        return artifactPath;
    }
}
