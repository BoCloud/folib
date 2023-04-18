package com.veadan.folib.eventlistener.webhook;

import cn.hutool.core.net.NetUtil;
import com.alibaba.fastjson.JSONObject;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.veadan.folib.components.artifact.ArtifactComponent;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.event.AsyncEventListener;
import com.veadan.folib.event.artifact.ArtifactEvent;
import com.veadan.folib.event.artifact.ArtifactEventTypeEnum;
import com.veadan.folib.forms.configuration.WebhookConfigurationForm;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.layout.DockerFileSystem;
import com.veadan.folib.services.WebhookService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * @author leipenghui
 * 事件监听，处理webhook
 **/
@Slf4j
@Component
public class ArtifactEventWebhookListener {

    @Autowired
    @Lazy
    private WebhookService webhookService;

    @Inject
    private ArtifactComponent artifactComponent;

    @AsyncEventListener
    public void handle(final ArtifactEvent<RepositoryPath> event) {
        int source = (int) event.getSource();
        RepositoryPath repositoryPath = event.getPath();
        ArtifactEventTypeEnum artifactEventTypeEnum = ArtifactEventTypeEnum.queryArtifactEventTypeEnumByType(source);
        log.debug("=====>>>>> {} 监听到制品事件：{}，path路径：{}", ArtifactEventWebhookListener.class.getSimpleName(), artifactEventTypeEnum, repositoryPath);
        if (Objects.isNull(artifactEventTypeEnum)) {
            return;
        }
        if (validateArtifactEvent(artifactEventTypeEnum) && artifactComponent.layoutSupports(repositoryPath)) {
            try {
                List<WebhookConfigurationForm> webhookConfigurationList = webhookService.getWebhookConfiguration();
                if (CollectionUtils.isEmpty(webhookConfigurationList)) {
                    log.debug("webhook尚未配置，无后续操作");
                    return;
                }
                Artifact artifact = repositoryPath.getArtifactEntry();
                if (Objects.isNull(artifact) && ArtifactEventTypeEnum.EVENT_ARTIFACT_DIRECTORY_PATH_DELETED.getType() != source) {
                    log.debug("repositoryPath：{} artifact未从图库中找到，无后续操作", repositoryPath);
                    return;
                }
                Map<String, String> headerMap = null;
                String body = "", artifactPath = "";
                String storageAndRepository = String.format("%s/%s/", repositoryPath.getStorageId(), repositoryPath.getRepositoryId());
                String path = repositoryPath.toString();
                artifactPath = path.substring(path.indexOf(storageAndRepository) + storageAndRepository.length());
                JSONObject defaultBodyJson = new JSONObject();
                defaultBodyJson.put("artifactPath", artifactPath);
                body = defaultBodyJson.toJSONString();
                List<String> removeKeyList = Lists.newArrayList("artifactArchiveListing", "artifactCoordinates", "report", "tagSet");
                if (Objects.nonNull(artifact)) {
                    body = JSONObject.toJSONString(artifact);
                    JSONObject bodyJson = JSONObject.parseObject(body);
                    removeKeyList.forEach(bodyJson::remove);
                    body = bodyJson.toJSONString();
                    artifactPath = getArtifactPath(repositoryPath, artifact);
                }
                String instance = NetUtil.getLocalhost().getHostAddress();
                for (WebhookConfigurationForm webhookConfiguration : webhookConfigurationList) {
                    if (!webhookConfiguration.getEvents().contains(artifactEventTypeEnum.toString())) {
                        continue;
                    }
                    try {
                        headerMap = Maps.newHashMap();
                        headerMap.put("Content-Type", "application/json");
                        headerMap.put("User-Agent", "Folibrary");
                        headerMap.put("X-Folibrary-Event", artifactEventTypeEnum.toString());
                        headerMap.put("X-Folibrary-Instance", instance);
                        if (StringUtils.isNotBlank(webhookConfiguration.getAccessToken())) {
                            headerMap.put("X-Folibrary-Token", webhookConfiguration.getAccessToken());
                        }
                        webhookService.handlerWebhook(webhookConfiguration, repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), artifactPath, artifactEventTypeEnum.toString(), body, headerMap);
                    } catch (Exception ex) {
                        log.error("事件监听，处理webhook，事件类型：{} repositoryPath：{} 错误：{}", source, repositoryPath, ExceptionUtils.getStackTrace(ex));
                    }
                }
            } catch (Exception ex) {
                log.error("事件监听，处理webhook，事件类型：{} repositoryPath：{} 错误：{}", source, repositoryPath, ExceptionUtils.getStackTrace(ex));
            }
        }
    }

    /**
     * 校验制品事件类型是否为需要处理的类型
     *
     * @param artifactEventTypeEnum 制品事件类型
     * @return true 需要处理 false 不需要处理
     */
    private boolean validateArtifactEvent(ArtifactEventTypeEnum artifactEventTypeEnum) {
        List<Integer> list = Arrays.asList(ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_STORED.getType(), ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_UPDATED.getType(), ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_DOWNLOADED.getType(), ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_DOWNLOAD_BLOCKED.getType(), ArtifactEventTypeEnum.EVENT_ARTIFACT_PATH_DELETED.getType(), ArtifactEventTypeEnum.EVENT_ARTIFACT_DIRECTORY_PATH_DELETED.getType());
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
}
