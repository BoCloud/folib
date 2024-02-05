package com.veadan.folib.task;


import com.alibaba.fastjson.JSONObject;
import com.google.common.collect.Lists;
import com.veadan.folib.components.DistributedLockComponent;
import com.veadan.folib.components.artifact.ArtifactComponent;
import com.veadan.folib.components.promotion.ArtifactPromotionProvider;
import com.veadan.folib.components.promotion.ArtifactPromotionProviderRegistry;
import com.veadan.folib.configuration.UnionTargetRepositoryConfiguration;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.enums.PromotionStatusEnum;
import com.veadan.folib.enums.SafeLevelEnum;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.layout.DockerLayoutProvider;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.util.*;
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
    private DistributedLockComponent distributedLockComponent;

    @Inject
    @Lazy
    private ArtifactComponent artifactComponent;

    @Inject
    @Lazy
    private ArtifactPromotionProviderRegistry artifactPromotionProviderRegistry;

    @Inject
    @Lazy
    private RepositoryPathResolver repositoryPathResolver;

    @Scheduled(cron = "0 0/6 * * * ? ")
    public void run() {
        String lockName = "PromotionTask";
        long waitTime = 3L;
        log.info("Wait for the lock [{}]", lockName);
        if (distributedLockComponent.lock(lockName, waitTime)) {
            try {
                log.info("Locked for [{}]", lockName);
                //无需扫描、扫描完成
                List<String> safeLevelList = Lists.newArrayList(SafeLevelEnum.UNWANTED_SCAN.getLevel(), SafeLevelEnum.SCAN_COMPLETE.getLevel());
                //晋级失败
                List<String> promotionStatusList = Lists.newArrayList(PromotionStatusEnum.FAIL.getStatus());
                boolean promotionBlock = artifactComponent.promotionBlock();
                if (promotionBlock) {
                    //晋级阻断开启，等待晋级也需要晋级
                    promotionStatusList.add(PromotionStatusEnum.WAIT.getStatus());
                } else {
                    safeLevelList.add(SafeLevelEnum.UN_SCAN.getLevel());
                }
                long totalCount = artifactRepository.countPromotionMatchingByIndex(safeLevelList, promotionStatusList);
                if (totalCount <= 0) {
                    return;
                }
                int batchSize = 100;
                // 计算总页数
                int totalPages = (int) Math.ceil((double) totalCount / batchSize);
                Pageable pageable;
                for (int currentPage = 1; currentPage <= totalPages; currentPage++) {
                    log.info("TotalPages [{}] currentPage [{}] batchSize [{}]", totalPages, currentPage, batchSize);
                    if (currentPage == 1) {
                        pageable = PageRequest.of(currentPage, batchSize).first();
                    } else {
                        pageable = PageRequest.of(currentPage, batchSize).previous();
                    }
                    List<Artifact> artifactList = artifactRepository.findPromotionMatchingByIndex(safeLevelList, promotionStatusList, pageable);
                    if (CollectionUtils.isNotEmpty(artifactList)) {
                        boolean block;
                        String storageId, repositoryId, artifactPath = "";
                        Set<UnionTargetRepositoryConfiguration> unionTargetRepositories;
                        UnionTargetRepositoryConfiguration unionTargetRepository = null;
                        String nodePromotionStatus;
                        RepositoryPath repositoryPath = null;
                        for (Artifact artifact : artifactList) {
                            try {
                                storageId = artifact.getStorageId();
                                repositoryId = artifact.getRepositoryId();
                                block = artifactComponent.vulnerabilityBlock(artifact, null);
                                if (block) {
                                    log.info("存储空间 [{}] 所属仓库 [{}] 制品 [{}] 存在漏洞，满足安全策略配置中的阻断条件，取消晋级", storageId, repositoryId, artifact.getArtifactPath());
                                    artifactComponent.handlerArtifactPromotion("", artifact.getStorageId(), artifact.getRepositoryId(), artifact.getArtifactPath(), PromotionStatusEnum.BLOCK.getStatus());
                                    continue;
                                }
                                //开始晋级
                                unionTargetRepositories = artifactComponent.getUnionTargetRepositories(storageId, repositoryId);
                                if (CollectionUtils.isNotEmpty(unionTargetRepositories)) {
                                    Map<String, List<UnionTargetRepositoryConfiguration>> unionTargetRepositoryConfigurationGroupMap = Optional.ofNullable(unionTargetRepositories).orElse(Collections.emptySet()).stream().collect(Collectors.groupingBy(UnionTargetRepositoryConfiguration::getNode));
                                    //成功的节点跳过 其余的继续晋级
                                    for (String node : artifact.getPromotionNodes()) {
                                        try {
                                            node = node.split(",")[0];
                                            unionTargetRepository = unionTargetRepositoryConfigurationGroupMap.get(node).get(0);
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
                                            artifactPath = artifact.getArtifactPath();
                                            repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
                                            ArtifactPromotionProvider artifactPromotionProvider = artifactPromotionProviderRegistry.getProvider(unionTargetRepository.getType());
                                            artifactPromotionProvider.promotion(repositoryPath, artifactPath, unionTargetRepository);
                                        } catch (Exception ex) {
                                            log.error("存储空间：{} 仓库：{} 处理自动晋级，制品：{} 联邦仓库：{} 错误：{}", storageId, repositoryId, artifactPath, JSONObject.toJSONString(unionTargetRepository), ExceptionUtils.getStackTrace(ex));
                                            artifactComponent.handlerArtifactPromotion(node, artifact.getStorageId(), artifact.getRepositoryId(), artifact.getArtifactPath(), PromotionStatusEnum.FAIL.getStatus());
                                        }
                                    }
                                }
                            } catch (Exception ex) {
                                log.error("存储空间：{} 仓库：{} 处理自动晋级，制品：{} 错误：{}", artifact.getStorageId(), artifact.getRepositoryId(), artifactPath, ExceptionUtils.getStackTrace(ex));
                            }
                        }
                    }
                }
            } finally {
                distributedLockComponent.unLock(lockName, 3500L);
            }
        } else {
            log.info("LockName [{}] was not get lock", lockName);
        }
        log.info("Scheduled PromotionTask end");
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
