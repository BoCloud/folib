package com.veadan.folib.controllers.adapter.jfrog;

import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.components.webhook.WebhookEventsProvider;
import com.veadan.folib.components.webhook.WebhookEventsProviderRegistry;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.configuration.ConfigurationUtils;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.controllers.adapter.jfrog.dto.ArtifactData;
import com.veadan.folib.controllers.adapter.jfrog.dto.WebhookDto;
import com.veadan.folib.entity.Dict;
import com.veadan.folib.enums.JFrogEventTypeEnum;
import com.veadan.folib.enums.WebhookEventsTypeEnum;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.io.RootRepositoryPath;
import com.veadan.folib.providers.layout.LayoutFileSystemProvider;
import com.veadan.folib.security.exceptions.ExpiredTokenException;
import com.veadan.folib.security.exceptions.InvalidTokenException;
import com.veadan.folib.services.ArtifactResolutionService;
import com.veadan.folib.services.JfrogMigrateService;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.users.security.SecurityTokenProvider;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.codec.digest.MessageDigestAlgorithms;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.Resource;
import javax.inject.Inject;
import jakarta.servlet.http.HttpServletRequest;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Objects;

/**
 * @author veadan
 * @date 2024/2/26
 **/
@Slf4j
@RequestMapping("/artifactory")
@RestController
public class ArtifactWebHookController {

    @Inject
    private SecurityTokenProvider securityTokenProvider;

    @Inject
    protected ConfigurationManager configurationManager;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    protected ArtifactResolutionService artifactResolutionService;

    @Inject
    protected WebhookEventsProviderRegistry webhookEventsProviderRegistry;

    @Resource
    private JfrogMigrateService jfrogMigrateService;

    @PostMapping("/webhook")
    public ResponseEntity<Object> webhook(@RequestBody String data, HttpServletRequest request) {
        log.info("JFrog event data [{}]", data);
        try {
            WebhookDto webhookDto = JSONObject.parseObject(data, WebhookDto.class);
            String tokenKey = "X-jfrog-event-auth";
            String token = request.getHeader(tokenKey);
            log.info("JFrog event header token [{}] [{}]", tokenKey, token);
            if (StringUtils.isBlank(token)) {
                return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(String.format("The header parameter [%s] is required", tokenKey));
            }
            String repositoryKey = "X-repository";
            String repositoryHeader = request.getHeader(repositoryKey);
            // 存储空间固定且仓库同名
            String storageKey = "X-storage";
            String storageHeader = request.getHeader(storageKey);
            if (StringUtils.isBlank(storageHeader) && StringUtils.isBlank(repositoryHeader)) {
                return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(String.format("The either header parameter [%s] or [%s] must be passed", repositoryKey, storageKey));
            }
            securityTokenProvider.getClaims(token, true);
            if (!JFrogEventTypeEnum.needHandle(webhookDto.getEventType())) {
                log.info("JFrog event [{}] not need handle", webhookDto.getEventType());
                return ResponseEntity.ok("");
            }
            log.info("JFrog event header storage [{}] [{}] header repository [{}] [{}]", storageKey, storageHeader, repositoryKey, repositoryHeader);
            String storageId = "", repositoryId = "";
            if (StringUtils.isNotBlank(repositoryHeader)) {
                //固定仓库
                storageId = ConfigurationUtils.getStorageId(repositoryHeader, repositoryHeader);
                repositoryId = ConfigurationUtils.getRepositoryId(repositoryHeader);
            } else if (StringUtils.isNotBlank(storageHeader)) {
                //固定存储空间下的同名仓库
                storageId = storageHeader;
                repositoryId = webhookDto.getData().getRepoKey();
            }
            Storage storage = configurationManager.getStorage(storageId);
            if (Objects.isNull(storage)) {
                log.warn("JFrog event storage [{}] not found", storageId);
                return ResponseEntity.status(HttpStatus.NOT_FOUND).body(GlobalConstants.STORAGE_NOT_FOUND_MESSAGE);
            }
            if (Objects.isNull(storage.getRepository(repositoryId))) {
                log.warn("JFrog event storage [{}] repository [{}] not found", storageId, repositoryId);
                return ResponseEntity.status(HttpStatus.NOT_FOUND).body(GlobalConstants.REPOSITORY_NOT_FOUND_MESSAGE);
            }
            RootRepositoryPath rootRepositoryPath = repositoryPathResolver.resolve(storageId, repositoryId);
            ArtifactData artifactData = webhookDto.getData();
            RepositoryPath repositoryPath = rootRepositoryPath.resolve(artifactData.getPath());
            Dict dict = jfrogMigrateService.getWebhookSetting();
            if (Objects.isNull(dict)) {
                return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body("Cannot find JFrog artifact migrate info");
            }
            boolean exists = false;
            String currentDigest = "";
            if (JFrogEventTypeEnum.DEPLOYED.getType().equalsIgnoreCase(webhookDto.getEventType()) && StringUtils.isNotBlank(artifactData.getSha256()) && Files.exists(repositoryPath)) {
                LayoutFileSystemProvider provider = rootRepositoryPath.getFileSystem().provider();
                final RepositoryPath checksumPath = provider.getChecksumPath(repositoryPath, MessageDigestAlgorithms.SHA_256);
                if (Objects.nonNull(checksumPath) && Files.exists(checksumPath)) {
                    try {
                        currentDigest = Files.readString(checksumPath);
                    } catch (IOException e) {
                        log.error(ExceptionUtils.getStackTrace(e));
                    }
                    exists = artifactData.getSha256().equals(currentDigest);
                }
            }
            if (exists) {
                log.info("JFrog event repositoryPath [{}] [{}] [{}] digestAlgorithm [sha256] digest [{}] currentDigest [{}] exists skip sync", storageId, repositoryId, artifactData.getPath(), artifactData.getSha256(), currentDigest);
                return ResponseEntity.ok("");
            }
            log.info("JFrog event repositoryPath [{}] [{}] [{}] digestAlgorithm [sha256] digest [{}] currentDigest [{}] not exists", storageId, repositoryId, artifactData.getPath(), artifactData.getSha256(), currentDigest);
            WebhookEventsProvider webhookEventsProvider = webhookEventsProviderRegistry.getProvider(WebhookEventsTypeEnum.resolveType(repositoryPath.getRepository().getLayout()));
            boolean result = webhookEventsProvider.handler(webhookDto, repositoryPath, dict, 1);
            if (!result) {
                return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(String.format("Handle event error [%s]", data));
            }
            return ResponseEntity.ok("");
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
            if (ex instanceof ExpiredTokenException) {
                log.warn("JFrog event the token has expired");
                return ResponseEntity.status(HttpStatus.FORBIDDEN).body("The token has expired");
            } else if (ex instanceof InvalidTokenException) {
                log.warn("JFrog event the token is invalid");
                return ResponseEntity.status(HttpStatus.FORBIDDEN).body("The token is invalid");
            }
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(String.format("Handle event error [%s]", data));
        }
    }

}
