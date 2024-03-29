package com.veadan.folib.components.promotion;

import com.veadan.folib.components.layout.DockerComponent;
import com.veadan.folib.configuration.UnionTargetRepositoryConfiguration;
import com.veadan.folib.domain.ArtifactDispatch;
import com.veadan.folib.dto.TargetDispatchRepositoryDto;
import com.veadan.folib.enums.ArtifactoryRepositoryTypeEnum;
import com.veadan.folib.promotion.PromotionUtil;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.services.JFrogService;
import com.veadan.folib.util.RepositoryPathUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.nio.file.Files;
import java.util.List;
import java.util.Objects;

/**
 * @author leipenghui
 **/
@Slf4j
@Component
public class JFrogArtifactPromotionProvider implements ArtifactPromotionProvider {

    @Inject
    private ArtifactPromotionProviderRegistry artifactPromotionProviderRegistry;

    @Inject
    private JFrogService jFrogService;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private DockerComponent dockerComponent;

    @Inject
    private PromotionUtil promotionUtil;

    @PostConstruct
    @Override
    public void register() {
        artifactPromotionProviderRegistry.addProvider(ArtifactoryRepositoryTypeEnum.JFROG.getType(), this);
        log.info("Registered artifact promotion '{}' with alias '{}'.",
                getClass().getCanonicalName(), ArtifactoryRepositoryTypeEnum.JFROG.getType());
    }

    @Override
    public void promotion(RepositoryPath repositoryPath, String artifactPath, UnionTargetRepositoryConfiguration unionTargetRepositoryConfiguration) {
        String storageId = repositoryPath.getStorageId();
        String repositoryId = repositoryPath.getRepositoryId();
        artifactPath = artifactPath.replace(String.format("%s/%s/", storageId, repositoryId), "");
        log.info("存储空间：{} 仓库：{} 制品：{} 目标节点：{} 目标节点类型：{} 目标仓库：{} 目标路径：{} 满足晋级条件，开始晋级", storageId, repositoryId, artifactPath, unionTargetRepositoryConfiguration.getNode(), unionTargetRepositoryConfiguration.getType(), unionTargetRepositoryConfiguration.getRepositoryId(), artifactPath);
        jFrogService.uploadItem(unionTargetRepositoryConfiguration.getNode(), unionTargetRepositoryConfiguration.getRepositoryId(), repositoryPath, artifactPath, true);
    }

    @Override
    public void dispatch(ArtifactDispatch artifactDispatch) {
        String storageId = artifactDispatch.getSrcStorageId();
        String repositoryId = artifactDispatch.getSrcRepositoryId();
        String artifactPath = artifactDispatch.getPath().replace(String.format("%s/%s/", storageId, repositoryId), ""), uploadArtifactPath = "";
        RepositoryPath rootRepositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
        for (TargetDispatchRepositoryDto item : artifactDispatch.getTargetDispatchRepositoryList()) {
            log.info("存储空间：{} 仓库：{} 制品：{} 目标节点：{} 目标节点类型：{} 目标仓库：{}", storageId, repositoryId, artifactPath, item.getDispatchClusterEnName(), item.getArtifactoryRepositoryType(), item.getTargetRepositoryId());
            try {
                List<RepositoryPath> list = RepositoryPathUtil.getPaths(rootRepositoryPath.getRepository().getLayout(), rootRepositoryPath);
                if (CollectionUtils.isEmpty(list)) {
                    log.info("存储空间：{} 仓库：{} 制品：{} 目标节点：{} 目标节点类型：{} 目标仓库：{} 未找到需要分发的制品数据", storageId, repositoryId, artifactPath, item.getDispatchClusterEnName(), item.getArtifactoryRepositoryType(), item.getTargetRepositoryId());
                    continue;
                }
                for (RepositoryPath repositoryPath : list) {
                    if (!artifactRealExists(repositoryPath)) {
                        log.warn("存储空间：{} 仓库：{} 制品：{} 目标节点：{} 目标节点类型：{} 目标仓库：{} 制品源文件[{}]未找到", storageId, repositoryId, artifactPath, item.getDispatchClusterEnName(), item.getArtifactoryRepositoryType(), item.getTargetRepositoryId(), repositoryPath.toString());
                        continue;
                    }
                    uploadArtifactPath = RepositoryFiles.relativizePath(repositoryPath);
                    log.info("存储空间：{} 仓库：{} 制品：{} 目标节点：{} 目标节点类型：{} 目标仓库：{} 目标路径：{} 开始分发", storageId, repositoryId, artifactPath, item.getDispatchClusterEnName(), item.getArtifactoryRepositoryType(), item.getTargetRepositoryId(), uploadArtifactPath);
                    jFrogService.uploadItem(item.getDispatchClusterEnName(), item.getTargetRepositoryId(), repositoryPath, uploadArtifactPath, false);
                }
            } catch (Exception ex) {
                log.error("存储空间：{} 仓库：{} 制品：{} 目标节点：{} 目标节点类型：{} 目标仓库：{} 分发错误：{}", storageId, repositoryId, artifactPath, item.getDispatchClusterEnName(), item.getArtifactoryRepositoryType(), item.getTargetRepositoryId(), ExceptionUtils.getStackTrace(ex));
            }
        }
    }

    private boolean artifactRealExists(RepositoryPath repositoryPath) {
        try {
            return Objects.nonNull(repositoryPath) && Files.exists(repositoryPath) && Objects.nonNull(repositoryPath.getArtifactEntry());
        } catch (Exception ex) {
            log.error("判断制品是否存在发生错误：{}", ExceptionUtils.getStackTrace(ex));
            return false;
        }
    }
}
