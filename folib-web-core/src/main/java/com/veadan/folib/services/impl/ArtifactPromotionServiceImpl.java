package com.veadan.folib.services.impl;

import cn.hutool.core.io.FileUtil;
import cn.hutool.core.io.IORuntimeException;
import cn.hutool.core.io.IoUtil;
import cn.hutool.core.lang.UUID;
import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.google.common.collect.Lists;
import com.hazelcast.core.HazelcastInstance;
import com.hazelcast.map.IMap;
import com.veadan.folib.cloud.storage.s3fs.util.UriUtils;
import com.veadan.folib.components.IdGenerateUtils;
import com.veadan.folib.components.artifact.ArtifactComponent;
import com.veadan.folib.components.promotion.ArtifactPromotionProvider;
import com.veadan.folib.components.promotion.ArtifactPromotionProviderRegistry;
import com.veadan.folib.components.security.SecurityComponent;
import com.veadan.folib.constant.ArtifactSyncRecordStatusEnum;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.controllers.promotion.ArtifactPromotionController;
import com.veadan.folib.dispatch.ClusterDispatchNodeDto;
import com.veadan.folib.domain.AnalysisHtmlGetDirAndFilePath;
import com.veadan.folib.domain.ArtifactDispatch;
import com.veadan.folib.domain.ArtifactParse;
import com.veadan.folib.domain.ArtifactPromotion;
import com.veadan.folib.domain.PromotionFileRelativePath;
import com.veadan.folib.domain.PromotionNodeOption;
import com.veadan.folib.domain.PromotionRepositoryInfo;
import com.veadan.folib.domain.RepositoryInfo;
import com.veadan.folib.dto.ArtifactDto;
import com.veadan.folib.dto.ArtifactPromotionInfoDto;
import com.veadan.folib.dto.PromotionArtifactDto;
import com.veadan.folib.dto.PromotionNodeOptionDto;
import com.veadan.folib.dto.TargetDispatchRepositoryDto;
import com.veadan.folib.dto.TargetRepositoyDto;
import com.veadan.folib.entity.ArtifactSyncRecord;
import com.veadan.folib.entity.Dict;
import com.veadan.folib.enums.ArtifactSyncRecordOpsTypeEnum;
import com.veadan.folib.enums.ArtifactSyncRecordSyncModelEnum;
import com.veadan.folib.enums.ArtifactoryRepositoryTypeEnum;
import com.veadan.folib.enums.BusinessCodeEnum;
import com.veadan.folib.mapper.ArtifactSyncRecordMapper;
import com.veadan.folib.mapper.ArtifactSyncSlaveRecordMapper;
import com.veadan.folib.model.request.ArtifactPromotionNodeOptionCallbackReq;
import com.veadan.folib.model.request.ArtifactSliceDownloadInfoReq;
import com.veadan.folib.model.request.ArtifactSliceUploadReq;
import com.veadan.folib.model.request.ArtifactSliceUploadWebReq;
import com.veadan.folib.model.response.ArtifactSliceDownloadInfoRes;
import com.veadan.folib.model.response.ArtifactSliceUploadInfoRes;
import com.veadan.folib.promotion.ArtifactUploadTask;
import com.veadan.folib.promotion.PromotionUtil;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.layout.LayoutProviderRegistry;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.repository.MavenRepositoryFeatures;
import com.veadan.folib.scanner.common.exception.BusinessException;
import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.services.ArtifactMetadataService;
import com.veadan.folib.services.ArtifactPromotionService;
import com.veadan.folib.services.ArtifactResolutionService;
import com.veadan.folib.services.ArtifactWebService;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.services.DictService;
import com.veadan.folib.services.RepositoryManagementService;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import com.veadan.folib.users.userdetails.SpringSecurityUser;
import com.veadan.folib.util.MessageDigestUtils;
import com.veadan.folib.utils.FileUtils;
import com.veadan.folib.utils.PropertiesUtils;
import com.veadan.folib.utils.UrlUtils;
import com.veadan.folib.utils.UserUtils;
import com.veadan.folib.ws.client.handler.command.FolibWsClientArtifactPullCommand;
import com.veadan.folib.ws.common.FolibWsAction;
import com.veadan.folib.ws.common.FolibWsRunManageUtil;
import com.veadan.folib.ws.common.FolibWsRunManageV2;
import com.veadan.folib.ws.server.Command;
import com.veadan.folib.ws.server.Priority;
import com.veadan.folib.ws.server.WSMessageRequest;
import com.veadan.folib.ws.server.WSMessageResponse;
import com.veadan.folib.ws.server.manage.FolibWsServerRunManage;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.codec.digest.MessageDigestAlgorithms;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.fileupload.disk.DiskFileItem;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.maven.model.Model;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.multipart.commons.CommonsMultipartFile;

import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.Invocation;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.Response;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.FutureTask;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import static com.veadan.folib.utils.UrlUtils.parsePath;

/**
 * @author qijianping
 */
@Service
@Slf4j
public class ArtifactPromotionServiceImpl implements ArtifactPromotionService {

    private final String upLoadURI = "/api/artifact/folib/promotion/upload-files";
    private final String REPOSITORY_URL = "api/configuration/folib/storages";
    private final String pullURI = "/api/artifact/folib/promotion/pull-files";
    private final String getFileRelativePaths = "/api/artifact/folib/promotion/getFileRelativePaths";

    @Inject
    private RepositoryManagementService repositoryManagementService;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private ArtifactManagementService artifactManagementService;

    @Autowired
    private PromotionUtil promotionUtil;

    @Autowired
    private ThreadPoolTaskExecutor asyncPromotionPoolTaskExecutor;

    @Autowired
    private ProxyRepositoryConnectionPoolConfigurationService clientPool;

    @Inject
    protected ArtifactResolutionService artifactResolutionService;

    @Inject
    private ArtifactMetadataService artifactMetadataService;

    @Inject
    private LayoutProviderRegistry layoutProviderRegistry;

    @Inject
    private ArtifactRepository artifactRepository;

    @Inject
    private ArtifactPromotionProviderRegistry artifactPromotionProviderRegistry;

    @Value("${folib.temp}")
    private String tempPath;

    @Value("${folib.host:localhost}")
    private String host;

    @Value("${folib.port}")
    private int port;

    @Inject
    private DictService dictService;

    @Inject
    @Lazy
    private MavenRepositoryFeatures mavenRepositoryFeatures;

    @Inject
    private SecurityComponent securityComponent;

    @Inject
    @Lazy
    private ArtifactComponent artifactComponent;

    @Inject
    private ArtifactSyncRecordMapper artifactSyncRecordMapper;

    @Inject
    private ConfigurationManagementService configurationManagementService;

    @Inject
    @Lazy
    private FolibWsClientArtifactPullCommand wsClientArtifactPullCommand;

    @Inject
    @Lazy
    private IdGenerateUtils idGenerateUtils;
    @Inject
    private FolibWsRunManageV2 folibWsRunManageV2;
    @Inject
    private ArtifactSyncSlaveRecordMapper artifactSyncSlaveRecordMapper;

    @Inject
    private ArtifactWebService artifactWebService;

    @Inject
    private HazelcastInstance hazelcastInstance;

    @Override
    public ResponseEntity syncCopy(ArtifactPromotion artifactPromotion) {
        try {
            checkParam(artifactPromotion);
            final String srcStorageId = artifactPromotion.getSrcStorageId();
            final String srcRepositoryId = artifactPromotion.getSrcRepositoryId();
            Repository srcRepository = repositoryManagementService.getStorage(srcStorageId).getRepository(srcRepositoryId);

            // 多个目标仓库复制
            artifactPromotion.getTargetRepositoyList().forEach(x -> {
                String destStorageId = x.getTargetStorageId();
                String destRepositoryId = x.getTargetRepositoryId();
                log.info("Copy [{}] from [{}] [{}] to [{}] [{}]...", artifactPromotion.getPath(), srcStorageId, srcRepositoryId, destStorageId,
                        destRepositoryId);
                singleSyncCopy(artifactPromotion, srcRepository, destStorageId, destRepositoryId);
            });
        } catch (Exception e) {
            log.error("Copy path params [{}] error [{}]", JSONObject.toJSONString(artifactPromotion), ExceptionUtils.getStackTrace(e));
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(e.getMessage());
        }
        return ResponseEntity.ok("Artifact copy success");
    }

    @Override
    public ResponseEntity syncMove(ArtifactPromotion artifactPromotion) {

        try {
            checkParam(artifactPromotion);
            promotionUtil.executeMove(artifactPromotion);
        } catch (Exception e) {
            log.error("Move path params [{}] error [{}]", JSONObject.toJSONString(artifactPromotion), ExceptionUtils.getStackTrace(e));
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(e.getMessage());
        }
        return ResponseEntity.ok("Artifact move success");

    }

    @Override
    public ResponseEntity copy(ArtifactPromotion artifactPromotion) {
        try {
            checkParam(artifactPromotion);
            final String srcStorageId = artifactPromotion.getSrcStorageId();
            final String srcRepositoryId = artifactPromotion.getSrcRepositoryId();
            Repository srcRepository = repositoryManagementService.getStorage(srcStorageId).getRepository(srcRepositoryId);

            // 多个目标仓库复制
            artifactPromotion.getTargetRepositoyList().forEach(x -> {
                String destStorageId = x.getTargetStorageId();
                String destRepositoryId = x.getTargetRepositoryId();
                log.info("Copy [{}] from [{}] [{}] to [{}] [{}]...", artifactPromotion.getPath(), srcStorageId, srcRepositoryId, destStorageId,
                        destRepositoryId);
                singleCopy(artifactPromotion, srcRepository, destStorageId, destRepositoryId);
            });
        } catch (Exception e) {
            log.error("Copy path params [{}] error [{}]", JSONObject.toJSONString(artifactPromotion), ExceptionUtils.getStackTrace(e));
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(e.getMessage());
        }
        return ResponseEntity.ok("Artifact copying");
    }

    private void checkParam(ArtifactPromotion artifactPromotion) throws Exception {
        final String srcStorageId = artifactPromotion.getSrcStorageId();
        final String srcRepositoryId = artifactPromotion.getSrcRepositoryId();

        if (null == repositoryManagementService.getStorage(srcStorageId)) {
            throw new IllegalArgumentException("The source storageId does not exist!");
        }

        Repository srcRepository = repositoryManagementService.getStorage(srcStorageId).getRepository(srcRepositoryId);
        if (null == srcRepository) {
            throw new IllegalArgumentException("The source repositoryId does not exist!");
        }

        if (!RepositoryTypeEnum.HOSTED.getType().equalsIgnoreCase(srcRepository.getType())) {
            throw new IllegalArgumentException("The source repositoryId does not local");
        }
        artifactPromotion.setPath(UriUtils.decode(artifactPromotion.getPath()));
        final RepositoryPath srcRepositoryPath = repositoryPathResolver.resolve(srcRepository, artifactPromotion.getPath());
        if (!Files.exists(srcRepositoryPath)) {
            throw new IllegalArgumentException("The source path does not exist!");
        }
        List<TargetRepositoyDto> targetList = artifactPromotion.getTargetRepositoyList();

        if (CollectionUtils.isEmpty(targetList)) {
            throw new IllegalArgumentException("The target repository is empty");
        }
        StringBuilder stringBuilder = new StringBuilder();
        for (TargetRepositoyDto dto : targetList) {
            String targetStorageId = dto.getTargetStorageId();
            String targetRepositoryId = dto.getTargetRepositoryId();
            if (null == repositoryManagementService.getStorage(targetStorageId)) {
                stringBuilder.append("storage:").append(targetStorageId).append(" not exits");
                continue;
            }
            Repository targetRepository = repositoryManagementService.getStorage(targetStorageId).getRepository(targetRepositoryId);
            if (null == targetRepository) {
                stringBuilder.append(System.lineSeparator()).append(" repository:").append(targetRepositoryId).append(" not exits");
                continue;
            }
            if (!RepositoryTypeEnum.HOSTED.getType().equalsIgnoreCase(targetRepository.getType())) {
                stringBuilder.append(System.lineSeparator()).append(" repository:").append(targetRepositoryId).append(" does not local");
            }
        }
        if (StringUtils.isNotBlank(stringBuilder.toString())) {
            throw new IllegalArgumentException(stringBuilder.toString());
        }
    }

    private void singleCopy(ArtifactPromotion artifactPromotion, Repository srcRepository, String destStorageId, String destRepositoryId) {
        Repository destRepository = repositoryManagementService.getStorage(destStorageId).getRepository(destRepositoryId);
        RepositoryPath srcPath = repositoryPathResolver.resolve(srcRepository, artifactPromotion.getPath());
        RepositoryPath targetPath = promotionUtil.getTargetPath(artifactPromotion, srcPath, destRepository);
        promotionUtil.executeCopy(srcPath, srcRepository, targetPath, destRepository);
    }

    private void singleSyncCopy(ArtifactPromotion artifactPromotion, Repository srcRepository, String destStorageId, String destRepositoryId) {
        Repository destRepository = repositoryManagementService.getStorage(destStorageId).getRepository(destRepositoryId);
        RepositoryPath srcPath = repositoryPathResolver.resolve(srcRepository, artifactPromotion.getPath());
        RepositoryPath targetPath = promotionUtil.getTargetPath(artifactPromotion, srcPath, destRepository);
        promotionUtil.executeSyncCopy(srcPath, srcRepository, targetPath, destRepository);
    }

//    private RepositoryPath getTargetPath(ArtifactPromotion artifactPromotion, RepositoryPath srcPath ,Repository destRepository) {
//        if(ProductTypeEnum.Debian.getFoLibraryName().equals(srcPath.getRepository().getLayout())) {
//            try {
//                Map<String, String> coordinates = srcPath.getArtifactEntry().getArtifactCoordinates().getCoordinates();
//                String arrtString = DebianUtils.getArrtString(coordinates.get(DebianConstant.DISTRIBUTION), coordinates.get(DebianConstant.COMPONENT), coordinates.get(DebianConstant.ARCHITECTURE));
//                String target = artifactPromotion.getTargetPath()+";"+arrtString;
//                return artifactPromotion.getTargetPath() == null ? null : repositoryPathResolver.resolve(destRepository, target);
//            } catch (IOException e) {
//                throw new IllegalArgumentException("The source path does not exist!");
//            }
//        }else {
//            return  artifactPromotion.getTargetPath() == null ? null : repositoryPathResolver.resolve(destRepository, artifactPromotion.getTargetPath());
//
//        }
//    }

    private void singleFastSyncCopy(ArtifactPromotion artifactPromotion, Repository srcRepository, String destStorageId, String destRepositoryId) {
        Repository destRepository = repositoryManagementService.getStorage(destStorageId).getRepository(destRepositoryId);
        RepositoryPath srcPath = repositoryPathResolver.resolve(srcRepository, artifactPromotion.getPath());
        RepositoryPath targetPath = promotionUtil.getTargetPath(artifactPromotion, srcPath, destRepository);
        promotionUtil.executeFastSyncCopy(srcPath, srcRepository, targetPath, destRepository);
    }

    private void singleFastSyncMove(ArtifactPromotion artifactPromotion, Repository srcRepository, String destStorageId, String destRepositoryId) {
        Repository destRepository = repositoryManagementService.getStorage(destStorageId).getRepository(destRepositoryId);
        RepositoryPath srcPath = repositoryPathResolver.resolve(srcRepository, artifactPromotion.getPath());
        RepositoryPath targetPath = promotionUtil.getTargetPath(artifactPromotion, srcPath, destRepository);
        promotionUtil.executeFastSyncMove(srcPath, srcRepository, targetPath, destRepository);
    }

    @Override
    public ResponseEntity move(ArtifactPromotion artifactPromotion) {
        try {
            checkParam(artifactPromotion);
            promotionUtil.executeMove(artifactPromotion);
        } catch (Exception e) {
            log.error("Move path params [{}] error [{}]", JSONObject.toJSONString(artifactPromotion), ExceptionUtils.getStackTrace(e));
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(e.getMessage());
        }
        return ResponseEntity.ok("Artifact moving");
    }

    @Override
    public CompletableFuture<Void> nodeOptionV2(PromotionNodeOption promotionNodeOption) {
        try {
            final String syncNo = promotionNodeOption.getSyncNo();
            PromotionRepositoryInfo promotionRepositoryInfo = resolvePromotionRepository(promotionNodeOption);
            String sourceStorageId = promotionRepositoryInfo.getSourceStorageId();
            String sourceRepositoryId = promotionRepositoryInfo.getSourceRepositoryId();
            String sourceBaseUrl = promotionRepositoryInfo.getSourceBaseUrl();
            String sourceArtifactPath = promotionRepositoryInfo.getSourceArtifactPath();
            String targetStorageId = promotionRepositoryInfo.getTargetStorageId();
            String targetRepositoryId = promotionRepositoryInfo.getTargetRepositoryId();
            String targetBaseUrl = promotionRepositoryInfo.getTargetBaseUrl();
            String targetArtifactPath = promotionRepositoryInfo.getTargetArtifactPath();
            if (sourceBaseUrl.equals(targetBaseUrl)) {
                validateStorageAndRepository(sourceStorageId, sourceRepositoryId);
                validateStorageAndRepository(targetStorageId, targetRepositoryId);
                Repository destRepository = repositoryManagementService.getStorage(targetStorageId).getRepository(targetRepositoryId);
                Repository srcRepository = repositoryManagementService.getStorage(sourceStorageId).getRepository(sourceRepositoryId);
                RepositoryPath srcPath = repositoryPathResolver.resolve(sourceStorageId, sourceRepositoryId, sourceArtifactPath);
                String targetPath = String.format("%s/%s/%s/%s", targetBaseUrl, targetStorageId, targetRepositoryId, targetArtifactPath);
                promotionUtil.executePromotionCopy(syncNo, targetPath, srcPath, srcRepository, destRepository);
                return CompletableFuture.completedFuture(null);
            }

            validateStorageAndRepository(sourceStorageId, sourceRepositoryId);

            // 本地源 制品路径 推向 目标路径
            Repository srcRepository = repositoryManagementService.getStorage(sourceStorageId).getRepository(sourceRepositoryId);
            RepositoryPath srcPath = repositoryPathResolver.resolve(srcRepository, sourceArtifactPath);
            //  遍历所有制品文件后逐步上传
            String srcAbsolutePath = srcPath.getTarget().toString();
            PromotionArtifactDto promotionArtifactDto = new PromotionArtifactDto(sourceStorageId, sourceRepositoryId,
                    targetStorageId, targetRepositoryId, srcAbsolutePath, targetBaseUrl + upLoadURI);

            PromotionNodeOptionDto uploadDto = promotionUtil.getPromotionUploadDto(promotionArtifactDto);
            uploadDto.setRetry(promotionNodeOption.isRetry());

            //CompletableFuture<Void> future = promotionUtil.artifactSliceUploadV3(uploadDto, targetBaseUrl, promotionNodeOption.getTargetNode(), targetStorageId, targetRepositoryId, syncNo);
            CompletableFuture<Void> future = promotionUtil.artifactSliceUploadV4(uploadDto, targetBaseUrl, promotionNodeOption.getTargetNode(), targetStorageId, targetRepositoryId, syncNo);
            return future;
        } catch (Exception e) {
            log.error("制品晋级错误 {}", ExceptionUtils.getStackTrace(e));
            if (e instanceof RuntimeException) {
                throw (RuntimeException) e;
            } else {
                throw new RuntimeException(e);
            }
        }
    }

    @Override
    @Deprecated
    public ResponseEntity<String> nodeOption(PromotionNodeOption promotionNodeOption, HttpServletRequest request) {
        try {
            String baseUrl = StringUtils.removeEnd(configurationManagementService.getConfiguration().getBaseUrl(), GlobalConstants.SEPARATOR);
            Integer syncModel = promotionNodeOption.getSyncModel();
            PromotionRepositoryInfo promotionRepositoryInfo = resolvePromotionRepository(promotionNodeOption);
            String sourceStorageId = promotionRepositoryInfo.getSourceStorageId();
            String sourceRepositoryId = promotionRepositoryInfo.getSourceRepositoryId();
            String sourceBaseUrl = promotionRepositoryInfo.getSourceBaseUrl();
            String sourceArtifactPath = promotionRepositoryInfo.getSourceArtifactPath();
            String targetStorageId = promotionRepositoryInfo.getTargetStorageId();
            String targetRepositoryId = promotionRepositoryInfo.getTargetRepositoryId();
            String targetBaseUrl = promotionRepositoryInfo.getTargetBaseUrl();
            if (sourceBaseUrl.equals(targetBaseUrl)) {
                validateStorageAndRepository(sourceStorageId, sourceRepositoryId);
                validateStorageAndRepository(targetStorageId, targetRepositoryId);
                Repository destRepository = repositoryManagementService.getStorage(targetStorageId).getRepository(targetRepositoryId);
                Repository srcRepository = repositoryManagementService.getStorage(sourceStorageId).getRepository(sourceRepositoryId);
                RepositoryPath srcPath = repositoryPathResolver.resolve(sourceStorageId, sourceRepositoryId, sourceArtifactPath);
                promotionUtil.executeCopy(srcPath, srcRepository, null, destRepository);
                return ResponseEntity.ok("ok");
            }
            if (Objects.isNull(syncModel)) {
                syncModel = ArtifactSyncRecordSyncModelEnum.PULL.getVal();
                if (sourceBaseUrl.startsWith(baseUrl)) {
                    syncModel = ArtifactSyncRecordSyncModelEnum.PUSH.getVal();
                }
            }
            if (ArtifactSyncRecordSyncModelEnum.PUSH.getVal().equals(syncModel)) {
                log.info("Use push model");
                validateStorageAndRepository(sourceStorageId, sourceRepositoryId);
                // 本地源 制品路径 推向 目标路径
                Repository srcRepository = repositoryManagementService.getStorage(sourceStorageId).getRepository(sourceRepositoryId);
                RepositoryPath srcPath = repositoryPathResolver.resolve(srcRepository, sourceArtifactPath);
                //  遍历所有制品文件后逐步上传
                String srcAbsolutePath = srcPath.getTarget().toString();
                PromotionArtifactDto promotionArtifactDto = new PromotionArtifactDto(sourceStorageId, sourceRepositoryId,
                        targetStorageId, targetRepositoryId, srcAbsolutePath, targetBaseUrl + upLoadURI);

                PromotionNodeOptionDto uploadDto = promotionUtil.getPromotionUploadDto(promotionArtifactDto);

//                //向目标仓库推包
                promotionUtil.upload(targetBaseUrl + upLoadURI, uploadDto);

            } else if (ArtifactSyncRecordSyncModelEnum.PULL.getVal().equals(syncModel)) {
                log.info("Use pull model");
                // 通过Ws协议通知客户端进行拉取操作
                final String targetHost = UrlUtils.getHost(targetBaseUrl);
                final Integer targetPort = UrlUtils.getPort(targetBaseUrl);
                final String nodeName = String.format("%s:%s", targetHost, targetPort);
                final FolibWsServerRunManage.FolibWsClientRun wsClientRun = FolibWsServerRunManage.getWsClientRun(nodeName);
                if (null == wsClientRun) {
                    if (targetBaseUrl.startsWith(baseUrl)) {
                        wsClientArtifactPullCommand.execute(promotionNodeOption);
                        return ResponseEntity.ok("ok");
                    } else {
                        promotionNodeOption.setSyncModel(ArtifactSyncRecordSyncModelEnum.PUSH.getVal());
                        return this.nodeOption(promotionNodeOption, request);
                    }
                }

                final FolibWsAction folibWsAction = new FolibWsAction()
                        .command(FolibWsClientArtifactPullCommand.COMMAND)
                        .payload(promotionNodeOption);
                wsClientRun.doAction(folibWsAction);
                // 表示通过拉取
                return ResponseEntity.ok(FolibWsClientArtifactPullCommand.COMMAND);
            }
        } catch (Exception e) {
            log.error("制品晋级错误 {}", ExceptionUtils.getStackTrace(e));
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(e.getMessage());
        }
        return ResponseEntity.ok("ok");
    }


    @Override
    public ResponseEntity nodeOptionAttachRecord(PromotionNodeOption promotionNodeOption, String requestHostName, HttpServletResponse response) {
        String targetHostName = FolibWsRunManageUtil.getSimpleTargetHostName(promotionNodeOption.getTargetPath());
        String sourceHostName = FolibWsRunManageUtil.getSimpleTargetHostName(promotionNodeOption.getSourcePath());
        String selfHostName = FolibWsRunManageUtil.getSimpleTargetHostName(configurationManagementService.getConfiguration().getBaseUrl());
        if (selfHostName.equals(sourceHostName)) {
            final String syncNo = String.format("SyncNo%s", UUID.randomUUID().toString(true));
            uploadArtifact(syncNo, promotionNodeOption, requestHostName);
            if (response.isCommitted()) {
                return null;
            }
            return ResponseEntity.ok(syncNo);
        } else if (selfHostName.equals(targetHostName)) {
            try {
                //委托sourceHostName节点upload到本节点，对本节点来说是下载，1小时超时时间，等待下载完成
                String sourceNodeName = FolibWsRunManageUtil.getTargetHostName(promotionNodeOption.getSourcePath());
                if (folibWsRunManageV2.forward(sourceNodeName)) {
                    return null;
                }
                WSMessageResponse wsMessageResponse = folibWsRunManageV2.sendRequest(sourceNodeName, new WSMessageRequest(Command.DELEGATE_UPLOAD, promotionNodeOption), 60 * 60);
                log.info("DelegateUpload WSMessageResponse:{}", wsMessageResponse);
                return ResponseEntity.ok("ok");
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }
        throw new RuntimeException("At least one of the hostname in the targetPath or sourcePath parameters must be " + selfHostName);
    }

    @Override
    public ResponseEntity<?> retryNodeOptionAttachRecord(String syncNo, HttpServletResponse response) {
        ArtifactSyncRecord artifactSyncRecord = artifactSyncRecordMapper.selectBySyncNo(syncNo);
        if (artifactSyncRecord == null) {
            throw new RuntimeException("Synchronization record not found");
        }
        PromotionNodeOption promotionNodeOption = new PromotionNodeOption();
        promotionNodeOption.setSourcePath(artifactSyncRecord.getSourcePath());
        promotionNodeOption.setTargetPath(artifactSyncRecord.getTargetPath());
        promotionNodeOption.setSyncModel(artifactSyncRecord.getSyncModel());
        promotionNodeOption.setSyncNo(syncNo);

        String targetHostName = FolibWsRunManageUtil.getSimpleTargetHostName(promotionNodeOption.getTargetPath());
        String sourceHostName = FolibWsRunManageUtil.getSimpleTargetHostName(artifactSyncRecord.getSourcePath());
        String selfHostName = FolibWsRunManageUtil.getSimpleTargetHostName(configurationManagementService.getConfiguration().getBaseUrl());
        promotionNodeOption.setTargetNode(targetHostName);
        promotionNodeOption.setRetry(true);
        if (selfHostName.equals(sourceHostName)) {
            retryUploadArtifact(syncNo, promotionNodeOption);
            if (Objects.nonNull(response) && response.isCommitted()) {
                return null;
            }
            return ResponseEntity.ok(syncNo);
        }
        throw new RuntimeException("At least one of the hostname in the targetPath or sourcePath parameters must be " + selfHostName);
    }

    @Override
    public CompletableFuture<Void> uploadArtifact(String syncNo, PromotionNodeOption promotionNodeOption, String requestHostName) {
        PromotionRepositoryInfo promotionRepositoryInfo = resolvePromotionRepository(promotionNodeOption);
        if (Objects.isNull(promotionNodeOption.getSyncModel())) {
            promotionNodeOption.setSyncModel(ArtifactSyncRecordSyncModelEnum.PUSH.getVal());
        }
        String targetNode = promotionNodeOption.getTargetNode();
        String sourceBaseUrl = promotionRepositoryInfo.getSourceBaseUrl();
        String targetBaseUrl = promotionRepositoryInfo.getTargetBaseUrl();
        if (!sourceBaseUrl.equals(targetBaseUrl)) {
            //不是同一个节点下的复制制品
            if (StringUtils.isBlank(targetNode)) {
                //无默认WS目标节点，解析WS目标节点
                targetNode = FolibWsRunManageUtil.getTargetNode(promotionNodeOption.getTargetPath());
                if (StringUtils.isBlank(targetNode)) {
                    //WS目标节点未找到，尝试转发到集群中其他节点处理
                    targetNode = FolibWsRunManageUtil.getTargetHostName(promotionNodeOption.getTargetPath());
                    if (folibWsRunManageV2.forward(targetNode)) {
                        return null;
                    }
                }
                promotionNodeOption.setTargetNode(targetNode);
            }
        }
        if (ArtifactSyncRecordSyncModelEnum.PUSH.getVal().equals(promotionNodeOption.getSyncModel())) {
            validateSourceRepositoryPath(promotionRepositoryInfo.getSourceStorageId(), promotionRepositoryInfo.getSourceRepositoryId(), promotionRepositoryInfo.getSourceArtifactPath());
            if (sourceBaseUrl.equals(targetBaseUrl)) {
                validateStorageAndRepository(promotionRepositoryInfo.getTargetStorageId(), promotionRepositoryInfo.getTargetRepositoryId());
            } else {
                validateRemoteRepository(targetNode, promotionRepositoryInfo.getTargetStorageId(), promotionRepositoryInfo.getTargetRepositoryId());
            }
        }
        String userName = UserUtils.getUsername();
        final ArtifactSyncRecord artifactSyncRecord = new ArtifactSyncRecord();

        // 生成日志记录
        artifactSyncRecord.setId(idGenerateUtils.generateId("artifactSyncRecordId"));
        artifactSyncRecord.setRequestHostName(requestHostName);
        artifactSyncRecord.setSourceStorageId(promotionRepositoryInfo.getSourceStorageId());
        artifactSyncRecord.setSourceRepositoryId(promotionRepositoryInfo.getSourceRepositoryId());
        artifactSyncRecord.setSourcePath(promotionNodeOption.getSourcePath());
        artifactSyncRecord.setTargetPath(promotionNodeOption.getTargetPath());
        artifactSyncRecord.setSyncNo(syncNo);
        artifactSyncRecord.setOpsType(ArtifactSyncRecordOpsTypeEnum.PROMOTION.getVal());
        artifactSyncRecord.setSyncModel(promotionNodeOption.getSyncModel());
        artifactSyncRecord.setStatus(ArtifactSyncRecordStatusEnum.READY.getVal());
        artifactSyncRecord.setCreateBy(userName);
        artifactSyncRecord.setCreateTime(new Date());
        artifactSyncRecordMapper.insertSelective(artifactSyncRecord);
        promotionNodeOption.setSyncNo(syncNo);
        try {
            return this.nodeOptionV2(promotionNodeOption);
        } catch (Exception e) {
            artifactSyncRecord.setStatus(ArtifactSyncRecordStatusEnum.FAILED.getVal());
            artifactSyncRecord.setFailedReason(e.getMessage());

            // 更新日志结束开始时间
            artifactSyncRecordMapper.updateByPrimaryKeySelective(artifactSyncRecord
                    .setUpdateTime(new Date())
                    .setUpdateBy(userName));
            if (e instanceof RuntimeException) {
                throw (RuntimeException) e;
            } else {
                throw new RuntimeException(e);
            }
        }
    }

    public CompletableFuture<Void> retryUploadArtifact(String syncNo, PromotionNodeOption promotionNodeOption) {

        PromotionRepositoryInfo promotionRepositoryInfo = resolvePromotionRepository(promotionNodeOption);
        if (Objects.isNull(promotionNodeOption.getSyncModel())) {
            promotionNodeOption.setSyncModel(ArtifactSyncRecordSyncModelEnum.PUSH.getVal());
        }
        String targetNode = promotionNodeOption.getTargetNode();
        String sourceBaseUrl = promotionRepositoryInfo.getSourceBaseUrl();
        String targetBaseUrl = promotionRepositoryInfo.getTargetBaseUrl();
        if (!sourceBaseUrl.equals(targetBaseUrl)) {
            //不是同一个节点下的复制制品
            if (StringUtils.isBlank(targetNode)) {
                //无默认WS目标节点，解析WS目标节点
                targetNode = FolibWsRunManageUtil.getTargetNode(promotionNodeOption.getTargetPath());
                if (StringUtils.isBlank(targetNode)) {
                    //WS目标节点未找到，尝试转发到集群中其他节点处理
                    targetNode = FolibWsRunManageUtil.getTargetHostName(promotionNodeOption.getTargetPath());
                    if (folibWsRunManageV2.forward(targetNode)) {
                        return null;
                    }
                }
                promotionNodeOption.setTargetNode(targetNode);
            }
        }
        if (ArtifactSyncRecordSyncModelEnum.PUSH.getVal().equals(promotionNodeOption.getSyncModel())) {
            validateSourceRepositoryPath(promotionRepositoryInfo.getSourceStorageId(), promotionRepositoryInfo.getSourceRepositoryId(), promotionRepositoryInfo.getSourceArtifactPath());
            if (sourceBaseUrl.equals(targetBaseUrl)) {
                validateStorageAndRepository(promotionRepositoryInfo.getTargetStorageId(), promotionRepositoryInfo.getTargetRepositoryId());
            } else {
                validateRemoteRepository(targetNode, promotionRepositoryInfo.getTargetStorageId(), promotionRepositoryInfo.getTargetRepositoryId());
            }
        }
        String userName = UserUtils.getUsername();

        final ArtifactSyncRecord artifactSyncRecord = artifactSyncRecordMapper.selectBySyncNo(syncNo);

        // 生成日志记录
        int retryCount = 0;
        if (Objects.nonNull(artifactSyncRecord.getRetryCount())) {
            retryCount = artifactSyncRecord.getRetryCount();
        }
        Date date = new Date();
        ArtifactSyncRecord updateArtifactSyncRecord = ArtifactSyncRecord.builder().id(artifactSyncRecord.getId()).status(ArtifactSyncRecordStatusEnum.READY.getVal()).retryCount(retryCount + 1).retryTime(date).updateBy(userName).updateTime(date).build();
        artifactSyncRecordMapper.updateByPrimaryKeySelective(updateArtifactSyncRecord);
        promotionNodeOption.setSyncNo(syncNo);
        try {
            return this.nodeOptionV2(promotionNodeOption);
        } catch (Exception e) {
            updateArtifactSyncRecord.setStatus(ArtifactSyncRecordStatusEnum.FAILED.getVal());
            updateArtifactSyncRecord.setFailedReason(e.getMessage());

            // 更新日志结束开始时间
            artifactSyncRecordMapper.updateByPrimaryKeySelective(updateArtifactSyncRecord);
            if (e instanceof RuntimeException) {
                throw (RuntimeException) e;
            } else {
                throw new RuntimeException(e);
            }
        }
    }

    @Override
    public Boolean artifactPullCallback(ArtifactPromotionNodeOptionCallbackReq model) {
        final String syncNo = model.getSyncNo();
        final Integer status = model.getStatus();
        final String failedReason = model.getFailedReason();
        if (StringUtils.isNotBlank(syncNo)) {
            artifactSyncRecordMapper.updateStatusAndFailedReasonBySyncNo(status, failedReason, syncNo, new Date());
        }

        return true;
    }

    @Override
    public ResponseEntity artifactPromotionInfo(String syncNo) {
        final ArtifactSyncRecord artifactSyncRecord = artifactSyncRecordMapper.selectOne(new ArtifactSyncRecord().setSyncNo(syncNo));
        if (null == artifactSyncRecord) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body("查询制品晋级信息不存在或已被删除");
        }

        final ArtifactPromotionInfoDto infoDto = new ArtifactPromotionInfoDto();
        BeanUtils.copyProperties(artifactSyncRecord, infoDto);
        return ResponseEntity.ok(infoDto);
    }

    @Override
    public ArtifactParse parseArtifact(String storageId, String repositoryId, MultipartFile file) {
        String uuid = UUID.fastUUID().toString();
        String fileOriginalName = ((CommonsMultipartFile) file).getFileItem().getName();
        String parentPath = "";
        ArtifactParse artifactParse = null;
        try (InputStream inputStream = file.getInputStream()) {
            parentPath = tempPath + File.separator + "parseArtifact" + File.separator + uuid;
            String artifactPath = parentPath + File.separator + fileOriginalName;
            File artifactFile = new File(artifactPath);
            FileUtil.writeFromStream(inputStream, artifactFile);
            Path path = Path.of(artifactFile.getAbsolutePath());
            if (artifactPath.endsWith(".pom")) {
                Model model = artifactComponent.getPom(path);
                String groupId = model.getGroupId();
                if (StringUtils.isBlank(groupId) && Objects.nonNull(model.getParent())) {
                    groupId = model.getParent().getGroupId();
                }
                String artifactId = model.getArtifactId();
                String version = model.getVersion();
                if (StringUtils.isBlank(version) && Objects.nonNull(model.getParent())) {
                    version = model.getParent().getVersion();
                }
                artifactParse = ArtifactParse.builder().type(1).groupId(groupId).artifactId(artifactId).version(version).filePath(artifactPath).build();
                return artifactParse;
            }
            byte[] propertiesBytes = PropertiesUtils.getFileFromJar(path, "pom.properties");
            if (Objects.isNull(propertiesBytes)) {
                artifactParse = ArtifactParse.builder().type(2).filePath(artifactPath).build();
                return artifactParse;
            }
            String properties = new String(propertiesBytes, StandardCharsets.UTF_8);
            String groupId = PropertiesUtils.parseProperties(properties, "groupId");
            String artifactId = PropertiesUtils.parseProperties(properties, "artifactId");
            String version = PropertiesUtils.parseProperties(properties, "version");
            artifactParse = ArtifactParse.builder().type(1).groupId(groupId).artifactId(artifactId).version(version).filePath(artifactPath).build();
            return artifactParse;
        } catch (Exception ex) {
            log.warn("解析制品错误：{}", ExceptionUtils.getStackTrace(ex));
            artifactParse = ArtifactParse.builder().type(2).build();
            return artifactParse;
        }
    }

    private AnalysisHtmlGetDirAndFilePath getArtifactPath(String url) throws Exception {
        Client client = clientPool.getRestClient();
        WebTarget target = client.target(url);
        Invocation.Builder builder = target.request();
        securityComponent.securityTokenHeader(builder);
        Response response = builder.get();
        if (response.getStatus() != 200) {
            throw new Exception("{} get error" + url);
        }
        Document doc = Jsoup.parse(response.readEntity(String.class));
        Elements tr = doc.body().children().get(1).getElementsByTag("tr");
        List<String> listDirPath = new ArrayList<>();
        List<String> listFilePath = new ArrayList<>();
        for (int i = 0; i < tr.size(); i++) {
            Element e1 = tr.get(i);
            Elements td = e1.getElementsByTag("td");
            if (td.size() == 0) {
                continue;
            }
            String value = td.get(0).text();
            if (!"-".equals(value) && !"..".equals(value)) {
                if (value.endsWith("/")) {
                    String temp = url + "/" + value;
                    temp = temp.substring(0, temp.length() - 1);
                    listDirPath.add(temp);
                } else {
                    listFilePath.add(url + "/" + value);
                    log.info("Waiting for processing pull file {}", url + "/" + value);
                }
            }
        }
        return AnalysisHtmlGetDirAndFilePath.builder().listFilePath(listFilePath).listDirPath(listDirPath).build();
    }

    @Override
    public ResponseEntity upload(MultipartFile[] files, String storageId, String repositoryId, String
            filePathMap, String fileMetaDataMap, String uuid, String imageTag, String fileType, String baseUrl, String token) {
        try {
            validateStorageAndRepository(storageId, repositoryId);
            List<FutureTask<String>> listTask = new ArrayList<>();
            Map<String, String> mapType = JSON.parseObject(filePathMap, Map.class);
            Map<String, Object> metaDataMap = StringUtils.isBlank(fileMetaDataMap) ?
                    new HashMap<>() : JSON.parseObject(fileMetaDataMap, Map.class);
            for (MultipartFile file : files) {
                //file.getOriginalFilename() 有问题修改用下面api
                String fileOriginalName = ((DiskFileItem) ((CommonsMultipartFile) file).getFileItem()).getName();
                String fileRelativePath = mapType.get(fileOriginalName);
                String metaData = metaDataMap.getOrDefault(fileRelativePath, "").toString();
                //ArtifactUploadTask artifactUploadTask = new ArtifactUploadTask(storageId, repositoryId, file,
                //        repositoryManagementService, repositoryPathResolver, artifactManagementService, promotionUtil, layoutProviderRegistry, artifactMetadataService, artifactRepository, mavenRepositoryFeatures, tempPath, fileRelativePath, metaData, uuid, null);
                ArtifactUploadTask artifactUploadTask = new ArtifactUploadTask(storageId, repositoryId, file,
                        repositoryManagementService, repositoryPathResolver, artifactManagementService, promotionUtil, layoutProviderRegistry, artifactMetadataService,
                        artifactRepository, mavenRepositoryFeatures, tempPath, fileRelativePath, metaData, uuid, null, imageTag, fileType, baseUrl, token);

                FutureTask<String> task = new FutureTask<String>(artifactUploadTask);
                listTask.add(task);
                asyncPromotionPoolTaskExecutor.submit(task);
            }
            StringBuilder temp = new StringBuilder();
            for (FutureTask<String> task : listTask) {
                try {
                    String resultMsg = task.get();
                    if (StringUtils.isNotBlank(resultMsg)) {
                        temp.append(resultMsg).append(System.lineSeparator());
                        log.error(resultMsg);
                    }

                } catch (Exception e) {
                    temp.append(e.getMessage()).append(System.lineSeparator());
                    log.error("upload exception {}", ExceptionUtils.getStackTrace(e));
                }
            }
            if (StringUtils.isNotBlank(temp.toString())) {
                return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                        .body(temp.toString());
            }
        } catch (Exception e) {
            log.error("upload exception", ExceptionUtils.getStackTrace(e));
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(e.getMessage());
        }
        return ResponseEntity.ok("ok");
    }

    @Override
    public ResponseEntity upload(String parseArtifact, String storageId, String repositoryId) {
        try {
            validateStorageAndRepository(storageId, repositoryId);
            ArtifactParse artifactParse = JSONObject.parseObject(parseArtifact, ArtifactParse.class);
            List<FutureTask<String>> listTask = new ArrayList<>();
            ArtifactUploadTask artifactUploadTask = new ArtifactUploadTask(storageId, repositoryId, null,
                    repositoryManagementService, repositoryPathResolver, artifactManagementService, promotionUtil, layoutProviderRegistry, artifactMetadataService, artifactRepository, mavenRepositoryFeatures, tempPath, FileUtil.getName(artifactParse.getFilePath()), null, null, parseArtifact);
            FutureTask<String> futureTask = new FutureTask<String>(artifactUploadTask);
            listTask.add(futureTask);
            asyncPromotionPoolTaskExecutor.submit(futureTask);
            StringBuilder temp = new StringBuilder();
            for (FutureTask<String> task : listTask) {
                try {
                    String resultMsg = task.get();
                    if (StringUtils.isNotBlank(resultMsg)) {
                        temp.append(resultMsg).append(System.lineSeparator());
                        log.error(resultMsg);
                    }

                } catch (Exception e) {
                    temp.append(e.getMessage()).append(System.lineSeparator());
                    log.error("upload exception {}", ExceptionUtils.getStackTrace(e));
                }
            }
            if (StringUtils.isNotBlank(temp.toString())) {
                return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                        .body(temp.toString());
            }
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(e.getMessage());
        }
        return ResponseEntity.ok("ok");
    }

    @Override
    public ResponseEntity download(ArtifactDto artifactDto, HttpServletResponse response) {
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(artifactDto.getStorageId(),
                artifactDto.getRepostoryId(),
                artifactDto.getPath());
        try (InputStream in = Files.newInputStream(repositoryPath);
        ) {
            OutputStream out = response.getOutputStream();
            response.setCharacterEncoding("UTF-8");
            // 设置文件头：设置下载文件名
            response.setHeader("Content-Disposition", "attachment;" + " filename=" + repositoryPath.getFileName().toString());
            int byteRead = 0;
            byte[] buffer = new byte[1024];
            while ((byteRead = in.read(buffer)) != -1) {
                out.write(buffer, 0, byteRead);
            }
            out.flush();
        } catch (IOException e) {
            log.error("download exception {}", ExceptionUtils.getStackTrace(e));
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(e.getMessage());
        }
        return ResponseEntity.ok("");
    }

    @Override
    public ResponseEntity getFileRelativePaths(ArtifactDto artifactDto) {
        try {
            // 获取路径下的所有文件
            validateStorageAndRepository(artifactDto.getStorageId(), artifactDto.getRepostoryId());
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(artifactDto.getStorageId(),
                    artifactDto.getRepostoryId(), artifactDto.getPath());
            PromotionFileRelativePath promotionFileRelativePath = promotionUtil.getFileRelativePaths(repositoryPath);
            return ResponseEntity.ok(promotionFileRelativePath);
        } catch (Exception e) {
            log.error("Get files relative paths exception {}", ExceptionUtils.getStackTrace(e));
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(e.getMessage());
        }
    }

    @Override
    public List<String> artifactDispatchAttachRecord(ArtifactDispatch artifactDispatch, HttpServletRequest request) {
        List<String> syncNoList = Lists.newArrayList();
        Map<String, List<TargetDispatchRepositoryDto>> groupByMap = artifactDispatch.getTargetDispatchRepositoryList().stream().collect(Collectors.groupingBy(TargetDispatchRepositoryDto::getArtifactoryRepositoryType));
        if (groupByMap.containsKey(ArtifactoryRepositoryTypeEnum.JFROG.getType())) {
            this.artifactDispatch(artifactDispatch);
            return syncNoList;
        }
        try {
            syncNoList = this.artifactDispatch(artifactDispatch);
        } catch (Exception e) {
            log.error("artifactDispatch exception", e);
            if (e instanceof RejectedExecutionException) {
                throw new RuntimeException("The promotion queue is full , info:" + e.getMessage());
            }
        }
        return syncNoList;
    }

    @Override
    public String retryArtifactDispatchAttachRecord(String syncNo, String type, HttpServletRequest request) {
        if (ArtifactoryRepositoryTypeEnum.JFROG.getType().equals(type)) {
            this.retryArtifactDispatch(syncNo, type);

        }
        try {
            this.retryArtifactDispatch(syncNo, type);
        } catch (Exception e) {
            log.error("retry artifactDispatch exception", e);
            if (e instanceof RejectedExecutionException) {
                throw new RuntimeException("The promotion queue is full , info:" + e.getMessage());
            }
        }
        return syncNo;
    }

    public void retryArtifactDispatch(String syncNo, String type) {
        log.info("Start retry artifact dispatch syncNo:{} ...", syncNo);
        ArtifactPromotionProvider artifactPromotionProvider = artifactPromotionProviderRegistry.getProvider(type);
        artifactPromotionProvider.retryDispatch(syncNo);
    }

    @Override
    public List<String> artifactDispatch(ArtifactDispatch artifactDispatch) {
        log.info("Start artifact dispatch [{}] ...", JSONObject.toJSONString(artifactDispatch));
        try {
            artifactDispatch.setPath(UriUtils.decode(artifactDispatch.getPath()));
        } catch (Exception ex) {
            log.warn(ExceptionUtils.getStackTrace(ex));
        }
        Map<String, List<TargetDispatchRepositoryDto>> groupByMap = artifactDispatch.getTargetDispatchRepositoryList().stream().collect(Collectors.groupingBy(TargetDispatchRepositoryDto::getArtifactoryRepositoryType));
        List<String> syncNoList = Lists.newArrayList(), itemSyncNoList;
        for (Map.Entry<String, List<TargetDispatchRepositoryDto>> item : groupByMap.entrySet()) {
            ArtifactPromotionProvider artifactPromotionProvider = artifactPromotionProviderRegistry.getProvider(item.getKey());
            ArtifactDispatch itemArtifactDispatch = new ArtifactDispatch();
            BeanUtils.copyProperties(artifactDispatch, itemArtifactDispatch);
            itemArtifactDispatch.setTargetDispatchRepositoryList(item.getValue());
            itemSyncNoList = artifactPromotionProvider.dispatch(itemArtifactDispatch);
            if (CollectionUtils.isNotEmpty(itemSyncNoList)) {
                syncNoList.addAll(itemSyncNoList);
            }
        }
        return syncNoList;
    }

    @Override
    public void validateStorageAndRepository(String storageId, String repositoryId) {
        if (null == repositoryManagementService.getStorage(storageId)) {
            throw new BusinessException("Storage [" + storageId + "] not exist!");
        }
        Repository repository = repositoryManagementService.getStorage(storageId).getRepository(repositoryId);
        if (null == repository) {
            throw new BusinessException("Repository [" + repositoryId + "]  not exist!");
        }
    }

    @Override
    public void validateRemoteRepository(String targetNode, String storageId, String repositoryId) {
        try {
            folibWsRunManageV2.sendRequest(targetNode, new WSMessageRequest(Command.CHECK_TARGET_NODE_REPOSITORY, RepositoryInfo.builder().storageId(storageId).repositoryId(repositoryId).build()));
        } catch (Exception ex) {
            log.error("Validate remote repository [{}] [{}] [{}] error [{}]", targetNode, storageId, repositoryId, ExceptionUtils.getStackTrace(ex));
            throw new BusinessException(ex.getMessage());
        }
    }

    @Override
    public List<Dict> queryUploadProcess(String dictType, String uuid) {
        return dictService.selectDict(Dict.builder().dictType(dictType).dictKey(uuid).build());
    }

    @Override
    public void deleteUploadProcess(String dictType, String uuid) {
        dictService.deleteDict(Dict.builder().dictType(dictType).dictKey(uuid).build());
    }

    /**
     * 节点下载连接数
     */
    public static final Map<String, AtomicInteger> DOWNLOAD_CONNECTION_COUNTER_MAP = new ConcurrentHashMap<>();

    @Override
    public Boolean speedLimitDownload(Repository repository, String artifactPath, String
            nodeMark, HttpServletResponse response) {
        // 获取全局节点限速
        final int kbps = Optional.ofNullable(configurationManagementService.getConfiguration().getKbps()).orElse(0) * (1024);
        // 获取节点限速
        final Collection<ClusterDispatchNodeDto> clusterDispatchNodeDtos = configurationManagementService.getMutableConfigurationClone().getClusterDispatchNode().values();
        final Map<String, Integer> nodeKbpsMap = clusterDispatchNodeDtos.stream().filter(e -> null == e.getAutoRegister() || !e.getAutoRegister()).collect(Collectors.toMap(e -> String.format("%s:%s", UrlUtils.getHost(e.getClusterNodeHost()), UrlUtils.getPort(e.getClusterNodeHost())), e -> null != e.getKbps() ? e.getKbps() * 1024 : 0));
        final int finalKbps = Optional.ofNullable(nodeKbpsMap.get(nodeMark)).filter(k -> k > 0).orElse(kbps);

        // 下载文件流
        InputStream sliceFileInputSteam = null;
        final RepositoryPath artifactRepositoryPath = repositoryPathResolver.resolve(repository, artifactPath);
        final String fileName = artifactRepositoryPath.getFileName().toString();
        response.setHeader("Content-Disposition", String.format("attachment;filename=%s", fileName));
        response.setContentType("application/x-gzip");

        try {
            if (Files.exists(artifactRepositoryPath)) {
                // Folib
                sliceFileInputSteam = Files.newInputStream(artifactRepositoryPath);
            } else {
                // Local-Temp（Slice file）
                final String storageId = repository.getStorage().getId();
                final String repositoryId = repository.getId();
                final String artifactFileSliceFilePath = String.format("%s/artifactSlice/%s/%s/%s", StringUtils.chomp(tempPath, "/"), storageId, repositoryId, artifactPath);
                final Path filePath = Path.of(artifactFileSliceFilePath);

                if (!Files.exists(filePath)) {
                    throw new BusinessException("下载的切片文件不存在或还未生成");
                }
                sliceFileInputSteam = Files.newInputStream(filePath);
            }
        } catch (IOException e) {
            log.error("获取下载文件流失败", e);
            return false;
        }
        log.info("FinalKbps [{}]", finalKbps);
        if (finalKbps > 0) {
            // 限速下载
            // - 获取初始下载速度
            AtomicInteger nodeDownloadConnectionCounter = DOWNLOAD_CONNECTION_COUNTER_MAP.get(nodeMark);
            if (null == nodeDownloadConnectionCounter) {
                nodeDownloadConnectionCounter = new AtomicInteger(0);
                DOWNLOAD_CONNECTION_COUNTER_MAP.put(nodeMark, nodeDownloadConnectionCounter);
            }

            try {
                this.sliceSpeedLimitDownload(sliceFileInputSteam, response.getOutputStream(), nodeDownloadConnectionCounter, finalKbps);
            } catch (Exception e) {
                log.error("限速下载文件失败", e);
                return false;
            } finally {
                nodeDownloadConnectionCounter.decrementAndGet();
            }
        } else {
            // 非限速下载
            try (final InputStream inputStream = sliceFileInputSteam;
                 final OutputStream outputStream = response.getOutputStream();) {
                IoUtil.copy(inputStream, outputStream);
            } catch (Exception e) {
                log.error("非限速下载文件失败", e);
                return false;
            }
        }

        return true;
    }

    @Override
    public Boolean speedLimitSliceDownload(Repository repository, String artifactPath, String nodeMark,
                                           String artifactMd5, Long startDownloadIndex, Long readLength,
                                           HttpServletResponse response) {
        // 获取全局节点限速
        final int kbps = Optional.ofNullable(configurationManagementService.getConfiguration().getKbps()).orElse(0) * (1024);
        // 获取节点限速
        final Collection<ClusterDispatchNodeDto> clusterDispatchNodeDtos = configurationManagementService.getMutableConfigurationClone().getClusterDispatchNode().values();
        final Map<String, Integer> nodeKbpsMap = clusterDispatchNodeDtos.stream().filter(e -> null == e.getAutoRegister() || !e.getAutoRegister()).collect(Collectors.toMap(e -> String.format("%s:%s", UrlUtils.getHost(e.getClusterNodeHost()), UrlUtils.getPort(e.getClusterNodeHost())), e -> null != e.getKbps() ? e.getKbps() * 1024 : 0));
        final int finalKbps = Optional.ofNullable(nodeKbpsMap.get(nodeMark)).filter(k -> k > 0).orElse(kbps);

        try {
            // 下载文件流
            final RepositoryPath artifactRepositoryPath = repositoryPathResolver.resolve(repository, artifactPath);
            if (!Files.exists(artifactRepositoryPath)) {
                throw new BusinessException("下载的文件不存在或已被删除");
            }
            final long fileSize = Files.size(artifactRepositoryPath);
            if (startDownloadIndex >= fileSize) {
                throw new BusinessException("下载的起始长度不能大于等于下载文件的最大长度");
            }
            final String artifactFileMd5 = Optional.ofNullable(artifactRepositoryPath.getArtifactEntry().getChecksums()).orElse(Collections.emptyMap()).get("MD5");
            if (!artifactMd5.equals(artifactFileMd5)) {
                throw new BusinessException("下载文件的MD5已经发生变化，请重写获取切片下载信息");
            }
            if (readLength > fileSize) {
                readLength = fileSize;
            }

            final String fileName = artifactRepositoryPath.getFileName().toString();
            response.setHeader("Content-Disposition", finalKbps > 0 ?
                    String.format("attachment;filename=%s-chunk-%s-%s", fileName, startDownloadIndex, readLength)
                    : fileName);
            response.setContentType("application/x-gzip");

            if (finalKbps > 0) {
                // 限速下载
                // - 获取初始下载速度
                AtomicInteger nodeDownloadConnectionCounter = DOWNLOAD_CONNECTION_COUNTER_MAP.get(nodeMark);
                if (null == nodeDownloadConnectionCounter) {
                    nodeDownloadConnectionCounter = new AtomicInteger(0);
                    DOWNLOAD_CONNECTION_COUNTER_MAP.put(nodeMark, nodeDownloadConnectionCounter);
                }

                try (final InputStream sliceFileInputSteam = Files.newInputStream(artifactRepositoryPath);
                     final OutputStream outputStream = response.getOutputStream();) {
                    int speedByteSize = this.getDownloadSpeedByte(finalKbps, nodeDownloadConnectionCounter.incrementAndGet());
                    if (speedByteSize > readLength) {
                        speedByteSize = Math.toIntExact(readLength);
                    }

                    sliceFileInputSteam.skip(startDownloadIndex);
                    final byte[] buffer = new byte[finalKbps];
                    long offset;
                    long totalOffset = 0;
                    while ((offset = sliceFileInputSteam.read(buffer, 0, speedByteSize)) != -1 & totalOffset < readLength) {
                        TimeUnit.SECONDS.sleep(1);
                        // 获取下一秒下载速度
                        speedByteSize = this.getDownloadSpeedByte(finalKbps, nodeDownloadConnectionCounter.get());
                        outputStream.write(buffer, 0, (int) offset);
                        totalOffset += offset;
                        if (totalOffset > readLength) {
                            speedByteSize = Math.toIntExact(readLength);
                        }
                    }
                } catch (Exception e) {
                    log.error("限速下载文件失败", e);
                    return false;
                } finally {
                    nodeDownloadConnectionCounter.decrementAndGet();
                }
            } else {
                // 非限速下载
                try (final InputStream inputStream = Files.newInputStream(artifactRepositoryPath);
                     final OutputStream outputStream = response.getOutputStream();) {
                    IoUtil.copy(inputStream, outputStream);
                } catch (Exception e) {
                    log.error("非限速下载文件失败", e);
                    return false;
                }
            }
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

        return true;
    }

    private int getDownloadSpeedByte(int limitKbps, int downloadThreadCount) {
        return limitKbps / downloadThreadCount;
    }

    private void sliceSpeedLimitDownload(InputStream inputStream, OutputStream outputStream, AtomicInteger
            downloadConnectionCounter, int finalKbps) {
        try (final OutputStream outputStream1 = outputStream;
             final InputStream inputStream1 = inputStream;) {
            int speedByteSize = this.getDownloadSpeedByte(finalKbps, downloadConnectionCounter.incrementAndGet());
            final byte[] speedBytes = new byte[finalKbps];
///            final String mark = UUID.randomUUID().toString(true);

            int offset;
            while ((offset = inputStream1.read(speedBytes, 0, speedByteSize)) != -1) {
                TimeUnit.SECONDS.sleep(1);
                // 获取下一秒下载速度
                speedByteSize = this.getDownloadSpeedByte(finalKbps, downloadConnectionCounter.get());
///                log.info("当前({})({})的下载速度为：{}", downloadConnectionCounter, mark, speedByteSize);
                outputStream1.write(speedBytes, 0, offset);
            }
        } catch (Exception e) {
            log.error("下载切片文件失败", e);
        }
    }

    /// @Override public Boolean querySupportSliceDownload(ArtifactSupportSliceDownloadQueryReq model) {
    ///        final String storageId = model.getStorageId();
    ///        final String repositoryId = model.getRepositoryId();
    ///        final String path = model.getPath();
    ///        final RepositoryPath artifactPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
    ///        if (!Files.exists(artifactPath)) {
    ///            throw new BusinessException("需要获取切片下载信息的制品不存在或已被删除");
    ///        }
    ///
    ///        try {
    ///            final long artifactFileLength = Files.size(artifactPath);
    ///            final long kbps = Optional.ofNullable(configurationManagementService.getConfiguration().getSliceMbSize()).orElse(0L) * (1024 * 1024);
    ///            return artifactFileLength > kbps;
    ///        } catch (Exception ex) {
    ///            log.error(ExceptionUtils.getStackTrace(ex));
    ///            throw new RuntimeException(ex);
    ///        }
    ///    }
    /// @Override public Map<String, Boolean> batchQuerySupportSliceDownload
    ///            (List<ArtifactSupportSliceDownloadQueryReq> models) {
    ///        final Map<String, Boolean> resultMap = new HashMap<>();
    ///        for (ArtifactSupportSliceDownloadQueryReq model : models) {
    ///            final String storageId = model.getStorageId();
    ///            final String repositoryId = model.getRepositoryId();
    ///            final String path = model.getPath();
    ///            final String fullPath = String.format("%s/%s/%s", storageId, repositoryId, path);
    ///            resultMap.put(fullPath, this.querySupportSliceDownload(model));
    ///        }
    ///
    ///        return resultMap;
    ///    }

    @Override
    public ArtifactSliceDownloadInfoRes querySliceDownloadInfo(ArtifactSliceDownloadInfoReq model) {
        final String storageId = model.getStorageId();
        final String repositoryId = model.getRepositoryId();
        final String path = model.getPath();
        final ArtifactSliceDownloadInfoRes artifactSliceDownloadInfoDto = new ArtifactSliceDownloadInfoRes();
        final RepositoryPath artifactPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
        if (!Files.exists(artifactPath)) {
            throw new BusinessException("需要获取切片下载信息的制品不存在或已被删除");
        }
        if (Files.isDirectory(artifactPath)) {
            throw new BusinessException("获取切片下载信息失败，目标是文件夹");
        }

        try {
            final long sliceByteSize = Optional.ofNullable(configurationManagementService.getConfiguration().getSliceMbSize()).orElse(0L) * (1024 * 1024);
            if (sliceByteSize <= 0) {
                throw new BusinessException("制品传输切片大小不能为空，请前往全局配置进行配置");
            }

            final long artifactFileLength = Files.size(artifactPath);
            final String artifactUri = String.format("%s/%s/%s", storageId, repositoryId, artifactPath.relativize());
            final String baseUrl = StringUtils.chomp(configurationManagementService.getConfiguration().getBaseUrl(), "/");
            final String md5 = null != artifactPath.getArtifactEntry() ? Optional.ofNullable(artifactPath.getArtifactEntry().getChecksums()).orElse(Collections.emptyMap()).get("MD5") : null;
            final int chunkCount = BigDecimal.valueOf(artifactFileLength).divide(BigDecimal.valueOf(sliceByteSize), 0, RoundingMode.CEILING).intValue();

            artifactSliceDownloadInfoDto.setStorageId(storageId);
            artifactSliceDownloadInfoDto.setRepositoryId(repositoryId);
            artifactSliceDownloadInfoDto.setPath(path);
            artifactSliceDownloadInfoDto.setUsedSlice(artifactFileLength > sliceByteSize);
            artifactSliceDownloadInfoDto.setArtifactMd5(md5);
            artifactSliceDownloadInfoDto.setDownloadPartList(new ArrayList<>());

            if (artifactSliceDownloadInfoDto.getUsedSlice()) {
                for (int i = 0; i < chunkCount; i++) {
                    // 计算每个线程的起始位置和结束位置
                    long startLength = i * sliceByteSize;
                    long endLength = (i == chunkCount - 1) ? artifactFileLength : startLength + sliceByteSize;

                    artifactSliceDownloadInfoDto.getDownloadPartList().add(
                            new ArtifactSliceDownloadInfoRes.DownloadPartInfo()
                                    .setSize(endLength - startLength)
                                    .setTemId(UUID.randomUUID().toString(true))
                                    .setDownloadUri(artifactUri)
                                    .setDownloadUrl(String.format("%s/api/artifact/folib/promotion/file/speedLimitSliceDownload/%s?artifactMd5=%s&startDownloadIndex=%s&readLength=%s", baseUrl, artifactUri, md5, startLength, sliceByteSize))
                    );
                }
            } else {
                artifactSliceDownloadInfoDto.getDownloadPartList().add(
                        new ArtifactSliceDownloadInfoRes.DownloadPartInfo()
                                .setSize(artifactFileLength)
                                .setTemId(UUID.randomUUID().toString(true))
                                .setDownloadUri(artifactUri)
                                .setDownloadUrl(String.format("%s/api/artifact/folib/promotion/file/speedLimitSliceDownload/%s?artifactMd5=%s&startDownloadIndex=0&readLength=%s", baseUrl, artifactUri, md5, artifactFileLength))
                );
            }

        } catch (Exception e) {
            log.error("获取制品切片下载信息失败", e);
            throw new BusinessException("获取制品切片下载信息失败");
        }

        return artifactSliceDownloadInfoDto;
    }

    /// @Override public ArtifactSliceDownloadInfoRes querySliceDownloadInfoStoreTemp(ArtifactSliceDownloadInfoReq model) {
    ///        final String storageId = model.getStorageId();
    ///        final String repositoryId = model.getRepositoryId();
    ///        final String path = model.getPath();
    ///        final ArtifactSliceDownloadInfoRes artifactSliceDownloadInfoDto = new ArtifactSliceDownloadInfoRes();
    ///        final RepositoryPath artifactPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
    ///        if (!Files.exists(artifactPath)) {
    ///            throw new BusinessException("需要获取切片下载信息的制品不存在或已被删除");
    ///        }
    ///        if (Files.isDirectory(artifactPath)) {
    ///            return null;
    ///        }
    ///
    ///        try {
    ///            final Repository repository = artifactPath.getRepository();
    ///            final Path fileName = artifactPath.getTarget().getFileName();
    ///            final String baseUrl = StringUtils.chomp(configurationManagementService.getConfiguration().getBaseUrl(), "/");
    ///            final String md5 = null != artifactPath.getArtifactEntry() ? Optional.ofNullable(artifactPath.getArtifactEntry().getChecksums()).orElse(Collections.emptyMap()).get("MD5") : null;
    ///            final long kbps = Optional.ofNullable(configurationManagementService.getConfiguration().getSliceMbSize()).orElse(0L) * (1024 * 1024);
    ///            if (kbps <= 0) {
    ///                throw new BusinessException("制品传输切片大小不能为空，请前往全局配置进行配置");
    ///            }
    ///
    ///            final long artifactFileLength = Files.size(artifactPath);
    ///            String artifactFilePath = artifactPath.toString();
    ///            final String artifactParentUri = Optional.of(artifactPath.relativize()).map(p -> {
    ///                try {
    ///                    return p.getParent().toString();
    ///                } catch (Exception e) {
    ///                    return StringUtils.EMPTY;
    ///                }
    ///            }).get();
    ///
    ///            artifactSliceDownloadInfoDto.setStorageId(storageId);
    ///            artifactSliceDownloadInfoDto.setRepositoryId(repositoryId);
    ///            artifactSliceDownloadInfoDto.setPath(path);
    ///            artifactSliceDownloadInfoDto.setUsedSlice(artifactFileLength > kbps);
    ///            artifactSliceDownloadInfoDto.setArtifactMd5(md5);
    ///
    ///            if (artifactSliceDownloadInfoDto.getUsedSlice()) {
    ///                try {
    ///                    final String sliceStoreFolderUri = String.format("%s.slice", StringUtils.isNotBlank(artifactParentUri) ? artifactParentUri + "/" : StringUtils.EMPTY);
    /// ///                    final String sliceGenJsonFileUri = String.format("%s/slice-gen.json", sliceStoreFolderUri);
    ///                    final String artifactFileSliceRootFolderPathStr = String.format("%s/artifactSlice/%s/%s", StringUtils.chomp(tempPath, "/"), storageId, repositoryId);
    ///                    final String artifactFileSliceFolderPathStr = String.format("%s/%s", artifactFileSliceRootFolderPathStr, sliceStoreFolderUri);
    /// ///                    final String sliceGenJsonFilePathStr = String.format("%s/%s", artifactFileSliceRootFolderPathStr, sliceGenJsonFileUri);
    ///
    ///                    if (S3FileSystemStorageProvider.ALIAS.equals(repository.getStorageProvider())) {
    ///                        // 由于是网络路径，需要暂存到本地进行暂存
    ///                        artifactFilePath = String.format("%s/artifactTemp/%s/%s", StringUtils.chomp(tempPath, "/"), UUID.randomUUID().toString(true), fileName);
    ///                        FileUtil.writeFromStream(new BufferedInputStream(Files.newInputStream(artifactPath)), artifactFilePath);
    ///                    }
    ///                    final List<String> splitFilePathList = FileUtils.splitFile(artifactFilePath, artifactFileSliceFolderPathStr, kbps);
    ///
    ///                    // 生成下载路径
    ///                    final List<ArtifactSliceDownloadInfoRes.DownloadPartInfo> downloadPartInfoList = splitFilePathList.stream()
    ///                            .map(splitFilePath -> {
    ///                                final String splitFileName = FileUtil.getName(splitFilePath);
    ///                                final String splitFileStoreUri = String.format("%s/%s", sliceStoreFolderUri, splitFileName);
    ///                                return new ArtifactSliceDownloadInfoRes.DownloadPartInfo()
    ///                                        .setDownloadUri(splitFileStoreUri)
    ///                                        /** {@linkplain ArtifactPromotionController#speedLimitDownload(Repository, String, String, HttpServletResponse)} */
    ///                                        .setDownloadUrl(String.format("%s/api/artifact/folib/promotion/file/speedLimitDownload/%s/%s/%s", baseUrl, storageId, repositoryId, splitFileStoreUri));
    ///                            })
    ///                            .collect(Collectors.toList());
    ///                    artifactSliceDownloadInfoDto.setDownloadPartList(downloadPartInfoList);
    ///
    ///                    // 持久化切片数据
    /// ///                    if (!Files.exists(sliceGenJsonFilePath)) {
    /// ///                        FileUtil.touch(sliceGenJsonFilePath.toFile());
    /// ///                    }
    /// ///                    Files.write(sliceGenJsonFilePath, JSON.toJSONString(artifactSliceDownloadInfoDto).getBytes(StandardCharsets.UTF_8));
    ///                } catch (IOException e) {
    ///                    log.error("切片制品文件失败", e);
    ///                    throw new BusinessException("切片制品文件失败");
    ///                }
    ///            } else {
    ///                final String artifactUri = String.format("%s/%s/%s", storageId, repositoryId, artifactPath.relativize());
    ///                artifactSliceDownloadInfoDto.setDownloadPartList(Collections.singletonList(
    ///                        new ArtifactSliceDownloadInfoRes.DownloadPartInfo()
    ///                                .setDownloadUri(artifactUri)
    ///                                .setDownloadUrl(String.format("%s/api/artifact/folib/promotion/file/speedLimitDownload/%s", baseUrl, artifactUri))
    ///                ));
    ///            }
    ///        } catch (BusinessException e) {
    ///            throw e;
    ///        } catch (Exception e) {
    ///            log.error("获取制品切片下载信息失败", e);
    ///            throw new BusinessException("获取制品切片下载信息失败");
    ///        }
    ///
    ///        return artifactSliceDownloadInfoDto;
    ///    }

    @Override
    public List<ArtifactSliceDownloadInfoRes> batchQuerySliceDownloadInfo
    (List<ArtifactSliceDownloadInfoReq> models) {
        return models.stream().map(this::querySliceDownloadInfo).filter(Objects::nonNull).collect(Collectors.toList());
    }

    @Override
    public ArtifactSliceUploadInfoRes querySliceUploadInfo() {
        final ArtifactSliceUploadInfoRes artifactSliceUploadInfoRes = new ArtifactSliceUploadInfoRes();
        artifactSliceUploadInfoRes.setMergeId(UUID.randomUUID().toString(true));
        final int chunkSize = Math.toIntExact(Optional.ofNullable(configurationManagementService.getConfiguration().getSliceMbSize()).orElse(0L));
        artifactSliceUploadInfoRes.setChunkSize(chunkSize);
        return artifactSliceUploadInfoRes;
    }

    @Override
    public Boolean sliceUpload(ArtifactSliceUploadReq model) {
        return sliceUpload(model, "");
    }

    @Override
    public Boolean sliceUpload(ArtifactSliceUploadReq model, String metaDataMap) {
        final String storageId = model.getStorageId();
        final String repositoryId = model.getRepositoryId();
        final String path = model.getPath();
        final MultipartFile file = model.getFile();
        final String mergeId = model.getMergeId();
        final Integer chunkNo = model.getChunkIndex();
        final Integer chunkNoMax = model.getChunkIndexMax();
        final String originFileMd5 = model.getOriginFileMd5();
        final String sliceMd5 = model.getSliceMd5();
        final Map<String, Object> metaData = Optional.ofNullable(model.getMetaData()).orElse(Collections.emptyMap());
        String metaDataJsonStr = JSON.toJSONString(metaData);
        if (StringUtils.isNotBlank(metaDataMap)) {
            metaDataJsonStr = metaDataMap;
        }
        // 临时存储目录
        final String artifactFileSliceUploadRootFolderPathStr = String.format("%s/artifactSliceUpload/%s/%s/%s", StringUtils.chomp(tempPath, "/"), storageId, repositoryId, mergeId);
        final String artifactFileSliceUploadFilePathStr = String.format("%s/chunkFile_%s", artifactFileSliceUploadRootFolderPathStr, chunkNo);
        final File artifactFileSliceUploadFile = new File(artifactFileSliceUploadFilePathStr);
        boolean allSliceFileUploadCompleted = false;
        boolean consistencyMd5 = false;
        AtomicBoolean allSliceFileDownloadCompleted = new AtomicBoolean(allSliceFileUploadCompleted);
        try {

            // 记录已上传的切片状态
            //final JSONObject sliceUploadStatusJSONObj = this.getSliceUploadStatusJSONObj(artifactFileSliceUploadRootFolderPathStr);

            // 检查当前切片是否已经上传，如果已上传则跳过
            //if (Files.exists(Path.of(artifactFileSliceUploadFilePathStr)) &&
            //        (sliceUploadStatusJSONObj.containsKey(String.valueOf(chunkNo)) && (Boolean) sliceUploadStatusJSONObj.get(String.valueOf(chunkNo)))) {
            //    log.info("Chunk {} already uploaded.", chunkNo);
            //    return true;
            //}

            if (Files.exists(Path.of(artifactFileSliceUploadFilePathStr))) {
                log.info("Chunk {} already uploaded.", chunkNo);

            } else {
                // 确保文件路径存在
                if (!FileUtil.exist(artifactFileSliceUploadFile)) {
                    // 创建空文件
                    log.info("Creating empty file: {}", artifactFileSliceUploadFilePathStr);
                    FileUtil.touch(artifactFileSliceUploadFile);
                }

                // 保存文件分片
                try (final InputStream inputStream = file.getInputStream();
                     final FileOutputStream fileOutputStream = new FileOutputStream(artifactFileSliceUploadFile)) {
                    IoUtil.copy(inputStream, fileOutputStream);
                    // 状态写入
                    //this.writeSliceUploadStatus(artifactFileSliceUploadRootFolderPathStr, chunkNo, true);
                    //log.info("Chunk {} uploaded.", chunkNo);
                } catch (IOException e) {
                    log.info("切片文件转存失败", e);
                    // 状态写入
                    //this.writeSliceUploadStatus(artifactFileSliceUploadRootFolderPathStr, chunkNo, false);
                    Files.deleteIfExists(Path.of(artifactFileSliceUploadFilePathStr));
                    throw new BusinessException(BusinessCodeEnum.ARTIFACT_SLICE_UPLOAD_CHUNK_FILE_SAVE_FAILED);
                }
            }


            // 检查所有切片是否都已上传完成
            //final JSONObject updatedSliceUploadStatusJSONObj = this.getSliceUploadStatusJSONObj(artifactFileSliceUploadRootFolderPathStr);
            // 通过判断上传完成的数量与最大切片块的数量确定是否所有切片文件都已上传完成
            //allSliceFileUploadCompleted = canMerger(artifactFileSliceUploadFile.length(), chunkNoMax, artifactFileSliceUploadRootFolderPathStr);
            allSliceFileUploadCompleted = canMerger(chunkNoMax, artifactFileSliceUploadRootFolderPathStr);
            allSliceFileDownloadCompleted.set(allSliceFileUploadCompleted);
            log.info("allSliceFileUploadCompleted: {}", allSliceFileUploadCompleted);
            if (allSliceFileDownloadCompleted.get()) {
                // 校验每个切片的上传状态
                //for (int i = 1; i <= chunkNoMax; i++) {
                //    if (!(Boolean) updatedSliceUploadStatusJSONObj.get(String.valueOf(i))) {
                //        throw new BusinessException(BusinessCodeEnum.ARTIFACT_SLICE_UPLOAD_CHUNK_FILE_UPLOAD_FAILED, String.valueOf(i));
                //    }
                //}

                // 进行合并操作
                final List<String> sliceFilePathList = IntStream.range(1, chunkNoMax + 1)
                        .mapToObj(i -> String.format("%s/chunkFile_%s", artifactFileSliceUploadRootFolderPathStr, i))
                        .map(p -> new File(p).getPath())
                        .collect(Collectors.toList());
                final RepositoryPath artifactFilePath = repositoryPathResolver.resolve(storageId, repositoryId, path);
                final String fileName = FileUtil.getName(artifactFilePath);
                final String mergeFilePath = String.format("%s/merge/%s", artifactFileSliceUploadRootFolderPathStr, fileName);

                final boolean mergeResult = FileUtils.mergeFiles(mergeFilePath, sliceFilePathList);
                log.info("mergeResult: {}", mergeResult);
                if (!mergeResult) {
                    throw new BusinessException(BusinessCodeEnum.ARTIFACT_SLICE_UPLOAD_CHUNK_FILE_MERGE_FAILED);
                }
                final String uploadArtifactFileMd5 = MessageDigestUtils.calculateChecksum(new File(mergeFilePath).toPath(), MessageDigestAlgorithms.MD5);
                // 校验MD5
                consistencyMd5 = originFileMd5.equals(uploadArtifactFileMd5);
                if (!consistencyMd5) {
                    throw new BusinessException(String.format("%s , originFileMd5:%s , uploadArtifactFileMd5:%s", BusinessCodeEnum.ARTIFACT_SLICE_UPLOAD_MD5_CHECK_FAILED.getMessage(), originFileMd5, uploadArtifactFileMd5));
                }

                // 转存合并文件到Folib
///                artifactManagementService.store(artifactFilePath, Files.newInputStream(Path.of(mergeFilePath)));

                FileStreamMultipartFile fileStreamMultipartFile = new FileStreamMultipartFile(new File(mergeFilePath), fileName, "", null);

                // 兼容原来上传逻辑
                final ArtifactUploadTask artifactUploadTask = new ArtifactUploadTask(storageId, repositoryId, fileStreamMultipartFile,
                        repositoryManagementService, repositoryPathResolver, artifactManagementService, promotionUtil,
                        layoutProviderRegistry, artifactMetadataService, artifactRepository, mavenRepositoryFeatures,
                        tempPath, path, metaDataJsonStr, null, null);
                final String result = artifactUploadTask.call();
                log.info("result: {}", result);
                if (StringUtils.isNotBlank(result)) {
                    throw new BusinessException(result);
                }
            }
        } catch (Exception e) {
            log.error("切片上传失败", e);
            throw new BusinessException(e.getMessage());
        } finally {
            log.info("SliceUploadTask finally");
            if (allSliceFileDownloadCompleted.get()) {
                log.info("SliceUploadTask finally delete temp file");
                try {
                    FileUtil.del(new File(artifactFileSliceUploadRootFolderPathStr));
                } catch (IORuntimeException e) {
                    log.error("删除临时文件 [{}] 失败 [{}]", artifactFileSliceUploadRootFolderPathStr, ExceptionUtils.getStackTrace(e));
                }
            }
        }

        return true;
    }

    private JSONObject getSliceUploadStatusJSONObj(String artifactFileSliceUploadRootFolderPathStr) {
        //final File sliceUploadStatusFile = new File(String.format("%s/sliceUploadStatus.json", artifactFileSliceUploadRootFolderPathStr));
        //// 检查文件是否存在
        //if (!sliceUploadStatusFile.exists()) {
        //    log.warn("Slice upload status file does not exist: {}", sliceUploadStatusFile.getPath());
        //    return new JSONObject(); // 返回一个空的 JSON 对象
        //}
        //return Optional.ofNullable(FileUtil.readString(sliceUploadStatusFile, StandardCharsets.UTF_8))
        //        .filter(StringUtils::isNotBlank)
        //        .map(JSON::parseObject)
        //        .orElse(new JSONObject());
        // 获取 Hazelcast 分布式 Map，假设 Map 名为 "uploadStatusMap"
        IMap<String, String> map = hazelcastInstance.getMap(artifactFileSliceUploadRootFolderPathStr);
        if (map.localKeySet().isEmpty()) {
            log.warn("Slice upload status map does not exist: {}", artifactFileSliceUploadRootFolderPathStr);
            return new JSONObject();
        } else {
            log.info("Slice upload status map exist: {}", artifactFileSliceUploadRootFolderPathStr);
            JSONObject result = new JSONObject();
            map.localKeySet().forEach(key -> result.put(key, Boolean.valueOf(map.get(key))));
            log.info("Slice upload status map content: {}", result.toString());
            return result;
        }

    }

    private void writeSliceUploadStatus(String artifactFileSliceUploadRootFolderPathStr, Integer chunkIndex, Boolean uploadStatus) {
        //final File sliceUploadStatusFile = new File(String.format("%s/sliceUploadStatus.json", artifactFileSliceUploadRootFolderPathStr));
        //
        //if (!FileUtil.exist(sliceUploadStatusFile)) {
        //    FileUtil.touch(sliceUploadStatusFile);
        //}
        //
        //final JSONObject uploadStatusJsonObj = Optional.ofNullable(FileUtil.readString(sliceUploadStatusFile, StandardCharsets.UTF_8))
        //        .filter(StringUtils::isNotBlank)
        //        .map(JSON::parseObject)
        //        .orElse(new JSONObject());
        //uploadStatusJsonObj.put(String.valueOf(chunkIndex), uploadStatus);
        //FileUtil.writeString(uploadStatusJsonObj.toJSONString(), sliceUploadStatusFile, StandardCharsets.UTF_8);

        log.info("Slice upload status map write: {}", artifactFileSliceUploadRootFolderPathStr);
        log.info("Slice upload status map write: {}", chunkIndex);
        log.info("Slice upload status map write: {}", uploadStatus);
        IMap<String, String> map = hazelcastInstance.getMap(artifactFileSliceUploadRootFolderPathStr);
        try {
            map.lock(String.valueOf(chunkIndex));
            map.put(String.valueOf(chunkIndex), Boolean.toString(uploadStatus), 2, TimeUnit.HOURS);
        } finally {
            map.unlock(String.valueOf(chunkIndex));
        }
        Map<String, String> localMapCopy = new HashMap<>(map);
        log.info("Slice upload status map content: {}", localMapCopy);
    }


    private PromotionRepositoryInfo resolvePromotionRepository(PromotionNodeOption promotionNodeOption) {
        try {
            PromotionRepositoryInfo promotionRepositoryInfo = PromotionRepositoryInfo.builder().build();
            String sourcePath = UriUtils.decode(StringUtils.removeEnd(promotionNodeOption.getSourcePath(), "/"));
            String targetPath = UriUtils.decode(StringUtils.removeEnd(promotionNodeOption.getTargetPath(), "/"));
            String sourceStorageId = parsePath(sourcePath)[0];
            String sourceRepositoryId = parsePath(sourcePath)[1];
            String sourceBaseUrl = sourcePath.split("/" + sourceStorageId + "/" + sourceRepositoryId + "/")[0];
            String sourceArtifactPath = sourcePath.split("/" + sourceStorageId + "/" + sourceRepositoryId + "/")[1];
            promotionRepositoryInfo.setSourceStorageId(sourceStorageId);
            promotionRepositoryInfo.setSourceRepositoryId(sourceRepositoryId);
            promotionRepositoryInfo.setSourceArtifactPath(sourceArtifactPath);
            promotionRepositoryInfo.setSourceBaseUrl(sourceBaseUrl);
            String targetStorageId = parsePath(targetPath)[0];
            String targetRepositoryId = parsePath(targetPath)[1];
            String targetBaseUrl = targetPath.split("/" + targetStorageId + "/" + targetRepositoryId + "/")[0];
            String targetArtifactPath = targetPath.split("/" + targetStorageId + "/" + targetRepositoryId + "/")[1];
            promotionRepositoryInfo.setTargetStorageId(targetStorageId);
            promotionRepositoryInfo.setTargetRepositoryId(targetRepositoryId);
            promotionRepositoryInfo.setTargetArtifactPath(targetArtifactPath);
            promotionRepositoryInfo.setTargetBaseUrl(targetBaseUrl);
            log.info("Promotion repository info [{}]", JSONObject.toJSONString(promotionRepositoryInfo));
            log.info("Source info sourcePath [{}] sourceBaseUrl [{}] storageId [{}] repositoryId [{}] artifactPath [{}]", sourcePath, sourceBaseUrl, sourceStorageId, sourceRepositoryId, sourceArtifactPath);
            log.info("Target info targetPath [{}] targetBaseUrl [{}] storageId [{}] repositoryId [{}] artifactPath [{}]", targetPath, targetBaseUrl, targetStorageId, targetRepositoryId, targetArtifactPath);
            return promotionRepositoryInfo;
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
            throw new BusinessException(String.format("resolve promotion repository info [%s] error [%s]", JSONObject.toJSONString(promotionNodeOption), ex.getMessage()));
        }
    }

    private void validateSourceRepositoryPath(String storageId, String repositoryId, String artifactPath) {
        validateStorageAndRepository(storageId, repositoryId);
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
        if (Objects.isNull(repositoryPath) || !Files.exists(repositoryPath)) {
            throw new BusinessException(String.format("Source repositoryPath [%s] [%s] [%s]  not exist!", storageId, repositoryId, artifactPath));
        }
    }

    /**
     * 更新任务队列优先级
     *
     * @param syncNo      同步编号
     * @param newPriority 优先级
     * @return
     */
    @Override
    public ResponseEntity<?> updateTaskQueuePriority(String syncNo, int newPriority) {
        // 获取分发配置信息
        Map<String, ClusterDispatchNodeDto> map = configurationManagementService.
                getMutableConfigurationClone().getClusterDispatchNode();
        ArtifactSyncRecord artifactSyncRecord = artifactSyncRecordMapper.selectBySyncNo(syncNo);
        if (artifactSyncRecord.getStatus() > 1) {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body("不是就绪状态不能置顶");
        }
        Priority priority = Priority.getPriority(newPriority);
        if (priority == null) {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body("优先级值不合法");
        }
        promotionUtil.updateTaskQueuePriority(syncNo, priority);

        return ResponseEntity.ok().build();
    }

    /**
     * 删除任务
     *
     * @param syncNo
     */
    @Override
    public ResponseEntity<?> deleteTask(String syncNo) {
        ArtifactSyncRecord artifactSyncRecord = artifactSyncRecordMapper.selectBySyncNo(syncNo);
        if (artifactSyncRecord.getStatus() == 3) {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body("任务已经结束不能删除");
        }
        if (artifactSyncRecord.getStatus() < 3) {
            promotionUtil.deleteTask(syncNo);
        }
        artifactSyncSlaveRecordMapper.deleteBySyncNo(syncNo);
        artifactSyncRecordMapper.delete(artifactSyncRecord);
        return ResponseEntity.ok().build();
    }

    /**
     * web切片上传
     *
     * @param model
     * @return
     */
    @Override
    public Boolean webSliceUpload(ArtifactSliceUploadWebReq model) {
        final String storageId = model.getStorageId();
        final String repositoryId = model.getRepositoryId();
        final String path = model.getPath();
        final MultipartFile file = model.getFile();
        final String mergeId = model.getMergeId();
        final Integer chunkNo = model.getChunkIndex();
        final Integer chunkNoMax = model.getChunkIndexMax();
        final String originFileMd5 = model.getOriginFileMd5();
        final String sliceMd5 = model.getSliceMd5();
        final Map<String, Object> metaData = Optional.ofNullable(model.getMetaData()).orElse(Collections.emptyMap());
        final String metaDataJsonStr = JSON.toJSONString(metaData);

        // 临时存储目录
        final String artifactFileSliceUploadRootFolderPathStr = String.format("%s/artifactSliceUpload/%s/%s/%s", StringUtils.chomp(tempPath, "/"), storageId, repositoryId, mergeId);
        final String artifactFileSliceUploadFilePathStr = String.format("%s/chunkFile_%s", artifactFileSliceUploadRootFolderPathStr, chunkNo);
        final File artifactFileSliceUploadFile = new File(artifactFileSliceUploadFilePathStr);
        boolean allSliceFileUploadCompleted = false;
        AtomicBoolean allSliceFileDownloadCompleted = new AtomicBoolean(false);

        try {

            // 记录已上传的切片状态
            //final JSONObject sliceUploadStatusJSONObj = this.getSliceUploadStatusJSONObj(artifactFileSliceUploadRootFolderPathStr);

            // 检查当前切片是否已经上传，如果已上传则跳过
            //if (Files.exists(Path.of(artifactFileSliceUploadFilePathStr)) &&
            //        sliceUploadStatusJSONObj.containsKey(String.valueOf(chunkNo)) && (Boolean) sliceUploadStatusJSONObj.get(String.valueOf(chunkNo))) {
            //    log.info("Chunk {} already uploaded.", chunkNo);
            //    return true;
            //}

            if (Files.exists(Path.of(artifactFileSliceUploadFilePathStr))) {
                log.info("Chunk {} already uploaded.", chunkNo);
            } else {
                // 确保文件路径存在
                if (!FileUtil.exist(artifactFileSliceUploadFile)) {
                    // 创建空文件
                    log.info("Create empty file {}", artifactFileSliceUploadFile);
                    FileUtil.touch(artifactFileSliceUploadFile);
                }

                // 保存文件分片
                try (final InputStream inputStream = file.getInputStream();
                     final FileOutputStream fileOutputStream = new FileOutputStream(artifactFileSliceUploadFile)) {
                    log.info("Chunk {} saved.", chunkNo);
                    IoUtil.copy(inputStream, fileOutputStream);
                    // 状态写入
                    //this.writeSliceUploadStatus(artifactFileSliceUploadRootFolderPathStr, chunkNo, true);
                } catch (IOException e) {
                    log.info("切片文件转存失败", e);
                    // 状态写入
                    //this.writeSliceUploadStatus(artifactFileSliceUploadRootFolderPathStr, chunkNo, false);
                    Files.deleteIfExists(Path.of(artifactFileSliceUploadFilePathStr));
                    throw new BusinessException(BusinessCodeEnum.ARTIFACT_SLICE_UPLOAD_CHUNK_FILE_SAVE_FAILED);
                }
            }


            log.info("All chunk files uploaded.");
            // 检查所有切片是否都已上传完成
            // final JSONObject updatedSliceUploadStatusJSONObj = this.getSliceUploadStatusJSONObj(artifactFileSliceUploadRootFolderPathStr);
            // 通过判断上传完成的数量与最大切片块的数量确定是否所有切片文件都已上传完成
            //allSliceFileUploadCompleted = canMerger(updatedSliceUploadStatusJSONObj.size(),  chunkNoMax, artifactFileSliceUploadRootFolderPathStr);
            allSliceFileUploadCompleted = canMerger(chunkNoMax, artifactFileSliceUploadRootFolderPathStr);
            allSliceFileDownloadCompleted.set(allSliceFileUploadCompleted);
            log.info("can merger:{}", allSliceFileUploadCompleted);
            if (allSliceFileDownloadCompleted.get()) {
                // 校验每个切片的上传状态
                //for (int i = 1; i <= chunkNoMax; i++) {
                //    if (!(Boolean) updatedSliceUploadStatusJSONObj.get(String.valueOf(i))) {
                //        throw new BusinessException(BusinessCodeEnum.ARTIFACT_SLICE_UPLOAD_CHUNK_FILE_UPLOAD_FAILED, String.valueOf(i));
                //    }
                //}

                log.info("All chunk files uploaded.");
                // 进行合并操作
                final List<String> sliceFilePathList = IntStream.range(1, chunkNoMax + 1)
                        .mapToObj(i -> String.format("%s/chunkFile_%s", artifactFileSliceUploadRootFolderPathStr, i))
                        .map(p -> new File(p).getPath())
                        .collect(Collectors.toList());

                log.info("Start to merge {} files.", chunkNoMax);
                final String fileName = model.getFileName();
                final String mergeFilePath = String.format("%s/merge/%s", artifactFileSliceUploadRootFolderPathStr, fileName);

                final boolean mergeResult = FileUtils.mergeFiles(mergeFilePath, sliceFilePathList);
                log.info("Merge result: {}", mergeResult);
                if (!mergeResult) {
                    throw new BusinessException(BusinessCodeEnum.ARTIFACT_SLICE_UPLOAD_CHUNK_FILE_MERGE_FAILED);
                }
                final String uploadArtifactFileMd5 = MessageDigestUtils.calculateChecksum(new File(mergeFilePath).toPath(), MessageDigestAlgorithms.MD5);
                // 校验MD5
                if (!originFileMd5.equals(uploadArtifactFileMd5)) {
                    throw new BusinessException(String.format("%s , originFileMd5:%s , uploadArtifactFileMd5:%s", BusinessCodeEnum.ARTIFACT_SLICE_UPLOAD_MD5_CHECK_FAILED.getMessage(), originFileMd5, uploadArtifactFileMd5));
                }
                log.info("MD5 check passed.");

                // 转存合并文件到Folib
///                artifactManagementService.store(artifactFilePath, Files.newInputStream(Path.of(mergeFilePath)));

                FileStreamMultipartFile fileStreamMultipartFile = new FileStreamMultipartFile(new File(mergeFilePath), fileName, model.getOriginalFilename(), null);

                if (model.isUnzip()) {
                    log.info("is Unzip file {}", mergeFilePath);
                    Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
                    SpringSecurityUser userDetails = (SpringSecurityUser) authentication.getPrincipal();
                    String filePath = StrUtil.isBlankOrUndefined(path) ? "" : path;
                    // 调用处理文件上传的方法
                    artifactWebService.store(userDetails.getUsername(), storageId, repositoryId, filePath, UUID.randomUUID().toString(), fileStreamMultipartFile);
                } else {
                    log.info("is Store file {}", mergeFilePath);
                    String filePath = StrUtil.isBlankOrUndefined(path) ? fileName : path.endsWith("/") ? String.join("", path, fileName) : String.join("/", path, fileName);
                    RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, filePath);
                    Repository repository = repositoryPath.getRepository();
                    if (Boolean.FALSE.equals(repository.isAllowsDeployment())) {
                        throw new BusinessException("deployment of artifacts to " +
                                repositoryPath.getStorageId() + ":" + repositoryPath.getRepositoryId() +
                                " repository is not allowed!");
                    }
                    if (Files.exists(repositoryPath) && Boolean.FALSE.equals(repository.isAllowsRedeployment())) {
                        throw new BusinessException("Re-deployment of artifacts to " +
                                repositoryPath.getStorageId() + ":" + repositoryPath.getRepositoryId() +
                                " repository is not allowed!");
                    }
                    // 兼容原来上传逻辑
                    final ArtifactUploadTask artifactUploadTask = new ArtifactUploadTask(storageId, repositoryId, fileStreamMultipartFile,
                            repositoryManagementService, repositoryPathResolver, artifactManagementService, promotionUtil, layoutProviderRegistry, artifactMetadataService,
                            artifactRepository, mavenRepositoryFeatures, tempPath, filePath, metaDataJsonStr, null, null, model.getImageTag(), model.getFileType(), model.getBaseUrl(), model.getToken());
                    final String result = artifactUploadTask.call();
                    if (StringUtils.isNotBlank(result)) {
                        throw new BusinessException(result);
                    }
                }

            }
        } catch (Exception e) {
            log.error("切片上传失败", e);
            throw new BusinessException(e.getMessage());
        } finally {
            log.info("SliceUploadTask finally");
            if (allSliceFileDownloadCompleted.get()) {
                log.info("SliceUploadTask finally delete temp file");
                try {
                    //IMap<String, String> map = hazelcastInstance.getMap(artifactFileSliceUploadRootFolderPathStr);
                    //log.info("Map size:{}",map.toString());
                    //map.destroy();
                    //log.info("Map deleted key:{}",artifactFileSliceUploadRootFolderPathStr);
                    FileUtil.del(new File(artifactFileSliceUploadRootFolderPathStr));
                } catch (IORuntimeException e) {
                    log.error("删除临时文件 [{}] 失败 [{}]", artifactFileSliceUploadRootFolderPathStr, ExceptionUtils.getStackTrace(e));
                }
            }
        }

        return true;
    }

    public long countChunkFiles(String artifactFileSliceUploadRootFolderPathStr) throws IOException {
        // 使用 try-with-resources 自动关闭流
        try (Stream<Path> files = Files.list(Path.of(artifactFileSliceUploadRootFolderPathStr))) {
            return files.filter(p -> p.getFileName().toString().startsWith("chunkFile_")).count();
        }
    }

    public boolean canMerger(long chunkNoMax, String artifactFileSliceUploadRootFolderPathStr) throws IOException {
        long chunkSize = countChunkFiles(artifactFileSliceUploadRootFolderPathStr);
        log.info("chunkSize:{},chunkNoMax:{}", chunkSize, chunkNoMax);
        boolean result = chunkSize == chunkNoMax;
        log.info("canMerger:{}", result);
        return result;
    }
}
