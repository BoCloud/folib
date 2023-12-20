package com.veadan.folib.promotion;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.file.Path;
import java.util.Date;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.collection.CollectionUtil;
import cn.hutool.core.lang.UUID;
import cn.hutool.core.map.MapUtil;
import cn.hutool.extra.spring.SpringUtil;
import cn.hutool.json.JSONUtil;
import com.alibaba.fastjson.JSON;
import com.google.common.collect.Lists;
import com.veadan.folib.artifact.coordinates.DockerArtifactCoordinates;
import com.veadan.folib.cloud.storage.s3fs.S3FileSystem;
import com.veadan.folib.cloud.storage.s3fs.S3Path;
import com.veadan.folib.components.artifact.ArtifactComponent;
import com.veadan.folib.components.layout.DockerComponent;
import com.veadan.folib.components.security.SecurityComponent;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.dispatch.ClusterDispatchNodeDto;
import com.veadan.folib.domain.*;
import com.veadan.folib.dto.*;
import com.veadan.folib.entity.ArtifactSyncSlaveRecord;
import com.veadan.folib.entity.Dict;
import com.veadan.folib.entity.License;
import com.veadan.folib.enums.ArtifactSyncRecordStatusEnum;
import com.veadan.folib.enums.ArtifactSyncRecordSyncModelEnum;
import com.veadan.folib.enums.PromotionStatusEnum;
import com.veadan.folib.enums.ThreadLocalContextFieldNameEnum;
import com.veadan.folib.forms.common.StorageTreeForm;
import com.veadan.folib.mapper.ArtifactSyncSlaveRecordMapper;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.layout.DockerLayoutProvider;
import com.veadan.folib.scanner.common.exception.BusinessException;
import com.veadan.folib.schema2.ImageManifest;
import com.veadan.folib.schema2.LayerManifest;
import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.veadan.folib.services.*;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.users.userdetails.SpringSecurityUser;
import com.veadan.folib.util.RepositoryPathUtil;
import com.veadan.folib.util.ThreadLocalUtil;
import com.veadan.folib.utils.FileUtils;
import com.veadan.folib.utils.UrlUtils;
import com.veadan.folib.wrapper.BufferedInputStreamWrapper;
import com.veadan.folib.ws.common.FolibWsAction;
import com.veadan.folib.ws.client.handler.command.FolibWsClientArtifactPullCommand;
import com.veadan.folib.ws.server.manage.FolibWsServerRunManage;
import lombok.Data;
import lombok.experimental.Accessors;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.mime.MultipartEntityBuilder;
import org.apache.http.entity.mime.content.InputStreamBody;
import org.apache.http.entity.mime.content.StringBody;
import org.apache.http.impl.client.HttpClients;
import org.glassfish.jersey.media.multipart.Boundary;
import org.glassfish.jersey.media.multipart.FormDataMultiPart;
import org.glassfish.jersey.media.multipart.file.StreamDataBodyPart;
import org.glassfish.jersey.media.multipart.internal.MultiPartWriter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.http.HttpStatus;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;
import tk.mybatis.mapper.entity.Example;

import javax.inject.Inject;
import javax.websocket.Session;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.Entity;
import javax.ws.rs.client.Invocation;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.Socket;
import java.nio.file.Files;
import java.util.*;
import java.util.concurrent.FutureTask;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

/**
 * @author qijianping
 */
@Component
@Slf4j
public class PromotionUtil {

    private final String upLoadURI = "/api/artifact/folib/promotion/upload-files";

    @Inject
    private RepositoryManagementService repositoryManagementService;

    @Inject
    protected ConfigurationManagementService configurationManagementService;

    @Inject
    protected ConfigurationManager configurationManager;
    @Inject
    protected ArtifactSyncSlaveRecordMapper artifactSyncSlaveRecordMapper;

    @Inject
    protected RepositoryPathResolver repositoryPathResolver;

    @Inject
    protected ArtifactResolutionService artifactResolutionService;

    @Inject
    protected ArtifactManagementService artifactManagementService;

    @Autowired
    private ThreadPoolTaskExecutor asyncThreadPoolTaskExecutor;

    @Autowired
    private ProxyRepositoryConnectionPoolConfigurationService clientPool;

    @Autowired
    private ArtifactService artifactService;

    @Autowired
    private ArtifactWebService artifactWebService;

    @Autowired
    private SecurityComponent securityComponent;

    @Autowired
    @Lazy
    private ArtifactComponent artifactComponent;

    @Autowired
    @Lazy
    private DockerComponent dockerComponent;

    @Async("asyncThreadPoolTaskExecutor")
    public void executeHanleCopy(String path, Repository destRepository, Repository srcRepository) {
        try {
            if (path.startsWith("s3://")) {
                handleS3ArtifactCopy(path, destRepository, srcRepository);
            } else {
                handleCopy(path, destRepository, srcRepository);
            }
            log.info("Artifact copyed [{}]", path);
        } catch (Exception e) {
            log.error("async handle copy artifact fail [{}]", ExceptionUtils.getStackTrace(e));
        }

    }

    @Async("asyncThreadPoolTaskExecutor")
    public void executeHandleDispatch(ArtifactDispatch artifactDispatch) {
        // 设置上下文字段
        ThreadLocalUtil.set(ThreadLocalContextFieldNameEnum.ARTIFACT_DISPATCH_SYNC_NO.getFieldName(), artifactDispatch.getSyncNo());

        // 获取分发配置信息
        Map<String, ClusterDispatchNodeDto> map = configurationManagementService.
                getMutableConfigurationClone().getClusterDispatchNode();
        if (MapUtil.isEmpty(map)) {
            log.error("分发配置为空!");
            return;
        }
        List<TargetDispatchRepositoryDto> targetRepositoryList = artifactDispatch.getTargetDispatchRepositoryList();
        String artifactPath = artifactDispatch.getPath();
        if (StringUtils.isBlank(artifactPath)) {
            log.warn("分发 path 不为空");
            return;
        }
        for (TargetDispatchRepositoryDto targetDispatchRepositoryDto : targetRepositoryList) {
            // 异步并发处理
            handlerDispatch(map, artifactDispatch, targetDispatchRepositoryDto);
        }
    }

    public void handlerDispatch(Map<String, ClusterDispatchNodeDto> map, ArtifactDispatch artifactDispatch,
                                TargetDispatchRepositoryDto targetDispatchRepositoryDto) {
        Response response = null;
        try {
            String artifactPath = artifactDispatch.getPath();
            String srcRepositoryId = artifactDispatch.getSrcRepositoryId();
            String srcStorageId = artifactDispatch.getSrcStorageId();
            String dispatchClusterName = targetDispatchRepositoryDto.getDispatchClusterEnName();
            String targetStorageId = targetDispatchRepositoryDto.getTargetStorageId();
            String targetRepositoryId = targetDispatchRepositoryDto.getTargetRepositoryId();
            String type = artifactDispatch.getType();
            String layout = artifactDispatch.getLayout();
            String policy = artifactDispatch.getPolicy();
            Boolean recordStatus = artifactDispatch.getRecordStatus();

            // 因三级联动插件原因 全选一级 或者二级 会得到 存储空间或者仓库为空的情况 ，如果仓库为空则需要再查一遍分发集群仓库信息。
            if (StringUtils.isBlank(dispatchClusterName)) {
                log.error("分发集群名不为空!");
                return;
            }
            ClusterDispatchNodeDto dispatchNodeDto = map.get(dispatchClusterName);
            if (null == dispatchNodeDto) {
                log.error("{} 分发配置不存在", dispatchClusterName);
                return;
            }
            if (StringUtils.isBlank(targetStorageId) || StringUtils.isBlank(targetRepositoryId)) {
                Map<String, ClusterDispatchNodeDto> dispatchMap = configurationManagementService.
                        getMutableConfigurationClone().getClusterDispatchNode();
                Client client = clientPool.getRestClient();
                ClusterDispatchNodeDto clusterDispatchNodeDto = dispatchMap.get(dispatchClusterName);
                ArtifactDispatchRepositoryDto dispatchRepositoryDto = ArtifactDispatchRepositoryDto.builder()
                        .type(type)
                        .layout(layout)
                        .dispatchEnName(dispatchClusterName)
                        .policy(policy).build();

                String host = clusterDispatchNodeDto.getClusterNodeHost();
                String url = host.endsWith("/") ? host + "api/configuration/folib/storages/getDispatchRepositories" :
                        host + "/api/configuration/folib/storages/getDispatchRepositories";
                WebTarget target = client.target(url);
                log.info(" 请求分发获取仓库信息 {}", JSONUtil.toJsonStr(dispatchRepositoryDto));
                Invocation.Builder builder = target.request();
                securityComponent.securityTokenHeader(builder);
                response = builder.post(Entity.entity(dispatchRepositoryDto, javax.ws.rs.core.MediaType.APPLICATION_JSON));
                if (response.getStatus() != 200) {
                    log.error("dispatch cluster {} get repositroy fail", dispatchClusterName);
                }
                DispatchStorageTree dispatchStorageTree = response.readEntity(DispatchStorageTree.class);
                List<StorageTreeForm> storageTreeForms = dispatchStorageTree.getList();
                if (StringUtils.isBlank(targetStorageId)) {
                    // 查询集群下全部的仓库（同类型 同策略 同布局）
                    for (StorageTreeForm storageTreeForm : storageTreeForms) {
                        List<StorageTreeForm> storages = storageTreeForm.getChildren();
                        if (CollectionUtil.isEmpty(storages)) {
                            continue;
                        }
                        for (StorageTreeForm storage : storages) {
                            targetStorageId = storage.getName();
                            List<StorageTreeForm> repos = storage.getChildren();
                            if (CollectionUtil.isEmpty(repos)) {
                                continue;
                            }
                            for (StorageTreeForm repo : repos) {
                                String tempRepoId = repo.getName();
                                executeDispatch(artifactPath, srcRepositoryId, srcStorageId, targetStorageId, tempRepoId, dispatchNodeDto, recordStatus);
                            }
                        }
                    }
                }
                if (StringUtils.isBlank(targetRepositoryId)) {
                    // 选存储空间下的全部仓库（同类型 同策略 同布局）
                    for (StorageTreeForm storageTreeForm : storageTreeForms) {
                        List<StorageTreeForm> storages = storageTreeForm.getChildren();
                        if (CollectionUtil.isEmpty(storages)) {
                            continue;
                        }
                        for (StorageTreeForm storage : storages) {
                            String tempStorage = storage.getName();
                            if (!tempStorage.equals(targetStorageId)) {
                                continue;
                            }
                            List<StorageTreeForm> repos = storage.getChildren();
                            if (CollectionUtil.isEmpty(repos)) {
                                continue;
                            }
                            for (StorageTreeForm repo : repos) {
                                String tempRepoId = repo.getName();
                                executeDispatch(artifactPath, srcRepositoryId, srcStorageId, targetStorageId, tempRepoId, dispatchNodeDto, recordStatus);
                            }
                            break;
                        }
                    }
                }
            } else {
                executeDispatch(artifactPath, srcRepositoryId, srcStorageId, targetStorageId, targetRepositoryId, dispatchNodeDto, recordStatus);
            }
        } catch (Exception e) {
            log.error("分发错误： {}", ExceptionUtils.getStackTrace(e));
        } finally {
            if (Objects.nonNull(response)) {
                response.close();
            }
        }
    }

    private void executeDispatch(String artifactPath, String srcRepositoryId, String srcStorageId, String targetStorageId, String targetRepositoryId, ClusterDispatchNodeDto dispatchNodeDto, Boolean recordStatus) {
        Response response = null;
        try {
            StringBuilder strBuilder = new StringBuilder();
            String dispatchNodeHost = dispatchNodeDto.getClusterNodeHost();
            strBuilder.append(dispatchNodeHost);
            if (dispatchNodeHost.endsWith("/")) {
                strBuilder.append(targetStorageId);
            } else {
                strBuilder.append("/");
                strBuilder.append(targetStorageId);
            }
            strBuilder.append("/").append(targetRepositoryId).append("/").append(artifactPath);
            String targetPath = strBuilder.toString();
            String baseUrl = configurationManagementService.getConfiguration().getBaseUrl();
            String sourcePath = baseUrl.endsWith("/") ? baseUrl + srcStorageId + "/" + srcRepositoryId + "/" + artifactPath :
                    baseUrl + "/" + srcStorageId + "/" + srcRepositoryId + "/" + artifactPath;
            String dispatchType = dispatchNodeDto.getDispatchType();
            PromotionNodeOption promotionNodeOption = null;
            final String syncNo = ThreadLocalUtil.get(ThreadLocalContextFieldNameEnum.ARTIFACT_DISPATCH_SYNC_NO.getFieldName(), String.class);
            final SpringSecurityUser userDetails = (SpringSecurityUser) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
            final String userName = Optional.ofNullable(userDetails).map(SpringSecurityUser::getUsername).orElse(null);
            
            log.info("分发 [{}] 开始", dispatchType);
            if (dispatchType.equals("pull")) {
                promotionNodeOption = new PromotionNodeOption(sourcePath, targetPath);
                promotionNodeOption.setSyncModel(ArtifactSyncRecordSyncModelEnum.PULL.getVal());
                promotionNodeOption.setSyncNo(syncNo);
                
                // 通过Ws协议通知客户端拉取制品
                final String clusterNodeHost = dispatchNodeDto.getClusterNodeHost();
                final String nodeHost = UrlUtils.getHost(clusterNodeHost);
                final Integer nodePort = UrlUtils.getPort(clusterNodeHost);
                final String nodeName = String.format("%s:%s", nodeHost, nodePort);
                final FolibWsServerRunManage.FolibWsClientRun wsClientRun = FolibWsServerRunManage.getWsClientRun(nodeName);
                if (null == wsClientRun) {
                    // 检查如果可以直接连接访问到目标节点，则将模式转换为push模式
                    try (final Socket socket = new Socket(nodeHost, nodePort);){
                        socket.setSoTimeout(200);
                        promotionNodeOption.setSyncModel(ArtifactSyncRecordSyncModelEnum.PUSH.getVal());
                        dispatchNodeDto.setDispatchType("push");
                        this.executeDispatch(artifactPath, srcRepositoryId, srcStorageId, targetStorageId, targetRepositoryId, dispatchNodeDto, recordStatus);
                    } catch (Exception e) {
                        throw new BusinessException("当前分发的节点不可用，请检查节点是否配置正确");
                    }
                    return;
                }

                final FolibWsAction folibWsAction = new FolibWsAction()
                        .command(FolibWsClientArtifactPullCommand.COMMAND)
                        .payload(promotionNodeOption);
                wsClientRun.doAction(folibWsAction);
            } else {
                Repository srcRepository = repositoryManagementService.getStorage(srcStorageId).getRepository(srcRepositoryId);
                RepositoryPath srcPath = repositoryPathResolver.resolve(srcRepository, artifactPath);
                //  遍历所有制品文件后逐步上传
                String srcAbsolutePath = srcPath.getTarget().toString();
                String targetUploadUrl = dispatchNodeHost.endsWith("/")
                        ? dispatchNodeHost + "api/artifact/folib/promotion/upload-files" :
                        dispatchNodeHost + "/api/artifact/folib/promotion/upload-files";
                PromotionArtifactDto promotionArtifactDto = new PromotionArtifactDto(srcStorageId, srcRepositoryId,
                        targetStorageId, targetRepositoryId, srcAbsolutePath, targetUploadUrl);
                PromotionNodeOptionDto uploadDto = getPromotionUploadDtoV2(promotionArtifactDto);

//                upload(targetUploadUrl, uploadDto);
                
                // 异步制品切片上传
                asyncThreadPoolTaskExecutor.submit(() -> {
                    try {
                        this.artifactSliceUpload(uploadDto, StringUtils.chomp(dispatchNodeHost, "/"), uploadDto.getStorageId(), uploadDto.getRepostoryId(), syncNo);
                    } catch (Exception e) {
                        log.error("异步制品切片上传失败", e);  
                    }
                });
                
                
                if (Boolean.TRUE.equals(recordStatus)) {
                    artifactComponent.handlerArtifactPromotion(dispatchNodeDto.getClusterEnName(), srcStorageId, srcRepositoryId, artifactPath, PromotionStatusEnum.SUCCESS.getStatus());
                }
            }
            log.info("分发 [{} {} {} {} {}] 成功 ", dispatchType, dispatchNodeDto.getClusterEnName(),
                    targetStorageId, targetRepositoryId, artifactPath);
        } catch (Exception e) {
            if (Boolean.TRUE.equals(recordStatus)) {
                artifactComponent.handlerArtifactPromotion(dispatchNodeDto.getClusterEnName(), srcStorageId, srcRepositoryId, artifactPath, PromotionStatusEnum.FAIL.getStatus());
            }
            log.error("分发 [{} {} {} {} {}] 失败 {} ",
                    dispatchNodeDto.getDispatchType(), dispatchNodeDto.getClusterEnName(),
                    targetStorageId, targetRepositoryId, artifactPath, ExceptionUtils.getStackTrace(e));
        } finally {
            if (Objects.nonNull(response)) {
                response.close();
            }
        }
    }

    @Async("asyncThreadPoolTaskExecutor")
    public void executeHandleMove(ArtifactPromotion artifactPromotion) {
        final String srcStorageId = artifactPromotion.getSrcStorageId();
        final String srcRepositoryId = artifactPromotion.getSrcRepositoryId();

        Repository srcRepository = repositoryManagementService.getStorage(srcStorageId).getRepository(srcRepositoryId);
        final RepositoryPath srcRepositoryPath = repositoryPathResolver.resolve(srcRepository, artifactPromotion.getPath());

        List<TargetRepositoyDto> list = artifactPromotion.getTargetRepositoyList();
        List<FutureTask<String>> listTask = new ArrayList<FutureTask<String>>();
        list.forEach(x -> {
            // 多个目标仓库移动
            String destStorageId = x.getTargetStorageId();
            String destRepositoryId = x.getTargetRepositoryId();
            Repository destRepository = repositoryManagementService.getStorage(destStorageId).getRepository(destRepositoryId);
            RepositoryPath srcPath = repositoryPathResolver.resolve(srcRepository, artifactPromotion.getPath());
            FutureTask<String> future = new FutureTask<String>(
                    new ArtifactPromotionCopyTask(srcPath.getTarget().toString(), destRepository, srcRepository));
            listTask.add(future);
            asyncThreadPoolTaskExecutor.submit(future);
        });
        boolean delFlag = true;
        for (FutureTask<String> task : listTask) {
            try {
                String rs = task.get();
                if (StringUtils.isNotBlank(rs)) {
                    delFlag = false;
                    log.error("Artitfact copy err {}", rs);
                }
            } catch (Exception e) {
                log.error("Exception {}", ExceptionUtils.getStackTrace(e));
            }
        }
        if (delFlag) {
            try {
                artifactManagementService.delete(srcRepositoryPath, false);
            } catch (IOException e) {
                log.error("async handle move artifact fail [{}]", ExceptionUtils.getStackTrace(e));
            }
        }
        log.info("Artifact moved [{}]", artifactPromotion.getPath());
    }

    public PromotionNodeOptionDto getPromotionUploadDto(PromotionArtifactDto promotionArtifactDto) throws Exception {
        PromotionNodeOptionDto promotionNodeOptionDto = new PromotionNodeOptionDto();
        promotionNodeOptionDto.setStorageId(promotionArtifactDto.getTargetStorageId());
        promotionNodeOptionDto.setRepostoryId(promotionArtifactDto.getTargetRepostoryId());
        Map<String, Map<String, InputStream>> filePathMap = new HashMap<>();
        Map<String, Object> fileMetaDataMap = new HashMap<>();
        if (promotionArtifactDto.getPath().startsWith("s3://")) {
            s3PromotionUpload(promotionArtifactDto, filePathMap, fileMetaDataMap);
        } else {
            nfsPromotionUpload(promotionArtifactDto, filePathMap, fileMetaDataMap);
        }
        promotionNodeOptionDto.setPathMap(filePathMap);
        promotionNodeOptionDto.setFileMetaDataMap(fileMetaDataMap);
        return promotionNodeOptionDto;
    }
    
    public PromotionNodeOptionDto getPromotionUploadDtoV2(PromotionArtifactDto promotionArtifactDto) throws Exception {
        PromotionNodeOptionDto promotionNodeOptionDto = new PromotionNodeOptionDto();
        promotionNodeOptionDto.setStorageId(promotionArtifactDto.getTargetStorageId());
        promotionNodeOptionDto.setRepostoryId(promotionArtifactDto.getTargetRepostoryId());
        Map<String, Map<String, InputStream>> fileInputStreamMap = new HashMap<>();
        Map<String, Object> fileMetaDataMap = new HashMap<>();
        Map<String, Map<String, Path>> filePathMap = new HashMap<>();
        
        if (promotionArtifactDto.getPath().startsWith("s3://")) {
            filePathMap = this.loadS3PromotionUploadFilePathMap(promotionArtifactDto, fileMetaDataMap);
        } else {
            filePathMap = this.loadNfsPromotionUploadFilePathMap(promotionArtifactDto, fileMetaDataMap);
        }
        filePathMap.forEach((pathStr, pathMap) -> {
            Map<String, InputStream> inputStreamPath = new HashMap<>();
            pathMap.forEach((k, v) -> {
                try {
                    inputStreamPath.put(k, Files.newInputStream(v));
                } catch (IOException e) {
                    throw new RuntimeException(e);
                }
            });
            fileInputStreamMap.put(pathStr, inputStreamPath);
        });
        
        promotionNodeOptionDto.setPathMap(fileInputStreamMap);
        promotionNodeOptionDto.setFilePathMap(filePathMap);
        promotionNodeOptionDto.setFileMetaDataMap(fileMetaDataMap);
        return promotionNodeOptionDto;
    }

    private void s3PromotionUpload(PromotionArtifactDto promotionArtifactDto, Map<String, Map<String, InputStream>> filePathInputSteamMap, Map<String, Object> fileMetaDataMap) throws Exception {
        final Map<String, Map<String, Path>> filePathMap = this.loadS3PromotionUploadFilePathMap(promotionArtifactDto, fileMetaDataMap);
        filePathMap.forEach((pathStr, pathMap) -> {
            Map<String, InputStream> inputStreamPath = new HashMap<>();
            pathMap.forEach((k, v) -> {
                try {
                    inputStreamPath.put(k, Files.newInputStream(v));
                } catch (IOException e) {
                    throw new RuntimeException(e);
                }
            });
            filePathInputSteamMap.put(pathStr, inputStreamPath);
        });

    }

    private Map<String, Map<String, Path>> loadS3PromotionUploadFilePathMap(PromotionArtifactDto promotionArtifactDto, Map<String, Object> fileMetaDataMap) throws Exception {
        final Map<String, Map<String, Path>> filePathMap = new HashMap<>();
        String absolutePath = promotionArtifactDto.getPath();
        String tempStr = promotionArtifactDto.getSrcStorageId() + File.separator + promotionArtifactDto.getSrcRepostoryId() + File.separator;
        int fPathIndex = absolutePath.lastIndexOf(tempStr);
        String relativizePath = absolutePath.substring(fPathIndex).replace(tempStr, "");
        RepositoryPath srcRepositoryPath = repositoryPathResolver.resolve(promotionArtifactDto.getSrcStorageId(), promotionArtifactDto.getSrcRepostoryId(), relativizePath);
        S3Path s3Path = new S3Path(SpringUtil.getBean(S3FileSystem.class), promotionArtifactDto.getPath());
        List<S3Path> s3FilesPaths = RepositoryPathUtil.getS3FiePaths(s3Path);
        if (DockerLayoutProvider.ALIAS.equalsIgnoreCase(srcRepositoryPath.getRepository().getLayout())) {
            s3FilesPaths = sortS3Docker(s3FilesPaths, srcRepositoryPath.getRepository().getLayout());
        }
        // 判断是否是docker 版本路径的复制
        boolean isDockerVersion = isDockerVersion(srcRepositoryPath.getRepository().getLayout(), s3FilesPaths.stream().map(S3Path::toString).collect(Collectors.toList()));
        if (isDockerVersion) {
            String[] arrayPath = relativizePath.split(File.separator);
            List<ImageManifest> imageManifestList = dockerComponent.getImageManifests(srcRepositoryPath);
            for (ImageManifest manifest : imageManifestList) {
                List<String> layerList = getAllLayerList(manifest);
                //blobs
                for (String layer : layerList) {
                    String blob = arrayPath[0] + File.separator + "blobs" + File.separator + layer;
                    RepositoryPath vSrcBlobPath = repositoryPathResolver.resolve(promotionArtifactDto.getSrcStorageId(), promotionArtifactDto.getSrcRepostoryId(), blob);
                    final String relativePath = this.getRelativePath(vSrcBlobPath.getTarget().toAbsolutePath().toString(), promotionArtifactDto.getSrcStorageId(), promotionArtifactDto.getSrcRepostoryId());
                    Map<String, Path> inputStreamMapBlobPath = new HashMap<>();
///                    inputStreamMapBlobPath.put(vSrcBlobPath.getTarget().toAbsolutePath().toString(), vSrcBlobPath);
                    inputStreamMapBlobPath.put(relativePath, vSrcBlobPath);
                    filePathMap.put(blob, inputStreamMapBlobPath);
                }
                if (StringUtils.isNotBlank(manifest.getDigest())) {
                    //manifest
                    String mainFestFile = arrayPath[0] + File.separator + "manifest" + File.separator + manifest.getDigest();
                    RepositoryPath srcMainFestPath = repositoryPathResolver.resolve(promotionArtifactDto.getSrcStorageId(), promotionArtifactDto.getSrcRepostoryId(), mainFestFile);
                    final String relativePath = this.getRelativePath(srcMainFestPath.getTarget().toAbsolutePath().toString(), promotionArtifactDto.getSrcStorageId(), promotionArtifactDto.getSrcRepostoryId());
                    Map<String, Path> inputStreamMapMainFestPath = new HashMap<>();
///                    inputStreamMapMainFestPath.put(srcMainFestPath.getTarget().toAbsolutePath().toString(), srcMainFestPath);
                    inputStreamMapMainFestPath.put(relativePath, srcMainFestPath);
                    filePathMap.put(mainFestFile, inputStreamMapMainFestPath);
                }
            }
        }
        for (S3Path s3FilePath : s3FilesPaths) {
            String relativePath = getRelativePath(s3FilePath.toAbsolutePath().toString(),
                    promotionArtifactDto.getSrcStorageId(),
                    promotionArtifactDto.getSrcRepostoryId());
            RepositoryPath srcPath = repositoryPathResolver.resolve(srcRepositoryPath.getStorageId(), srcRepositoryPath.getRepositoryId(), relativePath);
            if (RepositoryFiles.isChecksum(srcPath) || RepositoryFiles.isArtifactMetadata(srcPath)) {
                log.info(String.format("RepositoryPath：%s is checksum file skip", srcPath));
                continue;
            }
            boolean isDocker = DockerLayoutProvider.ALIAS.equalsIgnoreCase(srcRepositoryPath.getRepository().getLayout());
            if (isDocker && !srcPath.getFileName().toString().contains("sha256")) {
                log.info(String.format("RepositoryPath：%s not is docker layout file skip", srcPath));
                continue;
            }
            Map<String, Path> inputStreamMap = new HashMap<>();
///            inputStreamMap.put(s3FilePath.toAbsolutePath().toString(), s3FilePath);
            inputStreamMap.put(relativePath, s3FilePath);
            filePathMap.put(relativePath, inputStreamMap);
            // 添加跨节点的元数据同步
            fileMetaDataMap.put(relativePath, getMetaData(srcPath));
        }
        
        return filePathMap;
    }

    private void nfsPromotionUpload(PromotionArtifactDto promotionArtifactDto, Map<String, Map<String, InputStream>> filePathInputSteamMap, Map<String, Object> fileMetaDataMap) throws Exception {
        final Map<String, Map<String, Path>> filePathMap = this.loadNfsPromotionUploadFilePathMap(promotionArtifactDto, fileMetaDataMap);
        filePathMap.forEach((pathStr, pathMap) -> {
            Map<String, InputStream> inputStreamPath = new HashMap<>();
            pathMap.forEach((k, v) -> {
                try {
                    inputStreamPath.put(k, Files.newInputStream(v));
                } catch (IOException e) {
                    throw new RuntimeException(e);
                }
            });
            filePathInputSteamMap.put(pathStr, inputStreamPath);
        });
    }

    private Map<String, Map<String, Path>> loadNfsPromotionUploadFilePathMap(PromotionArtifactDto promotionArtifactDto, Map<String, Object> fileMetaDataMap) throws Exception {
        final Map<String, Map<String, Path>> filePathMap = new HashMap<>();
        String absolutePath = promotionArtifactDto.getPath();
        String tempStr = promotionArtifactDto.getSrcStorageId() + File.separator + promotionArtifactDto.getSrcRepostoryId() + File.separator;
        int fPathIndex = absolutePath.lastIndexOf(tempStr);
        String relativizePath = absolutePath;
        if (fPathIndex != -1) {
            relativizePath = absolutePath.substring(fPathIndex).replace(tempStr, "");
        }
        RepositoryPath srcRepositoryPath = repositoryPathResolver.resolve(promotionArtifactDto.getSrcStorageId(), promotionArtifactDto.getSrcRepostoryId(), relativizePath);
        List<File> list = RepositoryPathUtil.getNFSFiles(promotionArtifactDto.getPath());
        if (DockerLayoutProvider.ALIAS.equalsIgnoreCase(srcRepositoryPath.getRepository().getLayout())) {
            list = sortDocker(list, srcRepositoryPath.getRepository().getLayout());
        }
        // 判断是否是docker 版本路径的复制
        boolean isDockerVersion = isDockerVersion(srcRepositoryPath.getRepository().getLayout(), list.stream().map(File::getAbsolutePath).collect(Collectors.toList()));
        if (isDockerVersion) {
            String[] arrayPath = relativizePath.split(File.separator);
            List<ImageManifest> imageManifestList = dockerComponent.getImageManifests(srcRepositoryPath);
            if (CollectionUtils.isNotEmpty(imageManifestList)) {
                for (ImageManifest manifest : imageManifestList) {
                    List<String> layerList = getAllLayerList(manifest);
                    //blobs
                    for (String layer : layerList) {
                        String blob = arrayPath[0] + File.separator + "blobs" + File.separator + layer;
                        RepositoryPath vSrcBlobPath = repositoryPathResolver.resolve(promotionArtifactDto.getSrcStorageId(), promotionArtifactDto.getSrcRepostoryId(), blob);
                        final String relativePath = this.getRelativePath(vSrcBlobPath.getTarget().toAbsolutePath().toString(), promotionArtifactDto.getSrcStorageId(), promotionArtifactDto.getSrcRepostoryId());
                        Map<String, Path> inputStreamMapBlobPath = new HashMap<>();
///                        inputStreamMapBlobPath.put(vSrcBlobPath.getTarget().toAbsolutePath().toString(), vSrcBlobPath);
                        inputStreamMapBlobPath.put(relativePath, vSrcBlobPath);
                        filePathMap.put(blob, inputStreamMapBlobPath);
                    }
                    if (StringUtils.isNotBlank(manifest.getDigest())) {
                        //manifest
                        String mainFestFileStr = arrayPath[0] + File.separator + "manifest" + File.separator + manifest.getDigest();
                        RepositoryPath srcMainFestPath = repositoryPathResolver.resolve(promotionArtifactDto.getSrcStorageId(), promotionArtifactDto.getSrcRepostoryId(), mainFestFileStr);
                        final String relativePath = this.getRelativePath(srcMainFestPath.getTarget().toAbsolutePath().toString(), promotionArtifactDto.getSrcStorageId(), promotionArtifactDto.getSrcRepostoryId());
                        Map<String, Path> inputStreamMapMainFestPath = new HashMap<>();
///                        inputStreamMapMainFestPath.put(srcMainFestPath.getTarget().toAbsolutePath().toString(), srcMainFestPath);
                        inputStreamMapMainFestPath.put(relativePath, srcMainFestPath);
                        filePathMap.put(mainFestFileStr, inputStreamMapMainFestPath);
                    }
                }
            }
        }
        for (File file : list) {
            String relativePath = getRelativePath(file.getAbsolutePath(),
                    promotionArtifactDto.getSrcStorageId(),
                    promotionArtifactDto.getSrcRepostoryId());
            RepositoryPath srcPath = repositoryPathResolver.resolve(promotionArtifactDto.getSrcStorageId(), promotionArtifactDto.getSrcRepostoryId(), relativePath);
            if (RepositoryFiles.isChecksum(srcPath) || RepositoryFiles.isArtifactMetadata(srcPath)) {
                log.info(String.format("RepositoryPath：%s is checksum file skip", srcPath));
                continue;
            }
            boolean isDocker = DockerLayoutProvider.ALIAS.equalsIgnoreCase(srcRepositoryPath.getRepository().getLayout());
            if (isDocker && !srcPath.getFileName().toString().contains("sha256")) {
                log.info(String.format("RepositoryPath：%s not is docker layout file skip", srcPath));
                continue;
            }
            Map<String, Path> inputStreamMap = new HashMap<>();
///            inputStreamMap.put(file.getAbsolutePath(), file.toPath());
            inputStreamMap.put(relativePath, file.toPath());
            filePathMap.put(relativePath, inputStreamMap);
            // 添加跨节点的元数据同步
            fileMetaDataMap.put(relativePath, getMetaData(srcPath));
        }
        
        return filePathMap;
    }

    private String getRelativePath(String absolutePath, String storageId, String repostoryId) {
        String temp = storageId + "/" + repostoryId;
        int fPathIndex = absolutePath.lastIndexOf(temp + File.separator);
        return absolutePath.substring(fPathIndex, absolutePath.length()).replace(temp + File.separator, "");
    }

    public void handleCopy(String path, Repository destRepository, Repository srcRepository) throws Exception {
        List<File> list = RepositoryPathUtil.getNFSFiles(path);
        if (DockerLayoutProvider.ALIAS.equalsIgnoreCase(srcRepository.getLayout())) {
            list = sortDocker(list, srcRepository.getLayout());
        }
        // 判断是否是docker 版本路径的复制
        boolean isDockerVersion = isDockerVersion(srcRepository.getLayout(), list.stream().map(File::getAbsolutePath).collect(Collectors.toList()));
        if (isDockerVersion) {
            String tempStr = srcRepository.getStorage().getId() + File.separator + srcRepository.getId() + File.separator;
            int fPathIndex = path.lastIndexOf(tempStr);
            String relativizePath = path.substring(fPathIndex, path.length()).replace(tempStr, "");
            String[] arrayPath = relativizePath.split(File.separator);
            if (arrayPath.length != 2) {
                return;
            }
            List<File> fileContents = list.stream().filter(file -> DockerArtifactCoordinates.isManifestPath(file.getName())).collect(Collectors.toList());
            File file = fileContents.get(0);
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(srcRepository.getStorage().getId(), srcRepository.getId(), String.format("%s/%s", relativizePath, file.getName()));
            List<ImageManifest> imageManifestList = dockerComponent.getImageManifests(repositoryPath);
            if (CollectionUtils.isNotEmpty(imageManifestList)) {
                for (ImageManifest manifest : imageManifestList) {
                    List<String> layerList = getAllLayerList(manifest);
                    for (String layer : layerList) {
                        String blob = arrayPath[0] + File.separator + "blobs" + File.separator + layer;
                        RepositoryPath vSrcBlobPath = repositoryPathResolver.resolve(srcRepository.getStorage().getId(), srcRepository.getId(), blob);
                        try (InputStream blobIs = Files.newInputStream(vSrcBlobPath)) {
                            RepositoryPath destBlobPath = repositoryPathResolver.resolve(destRepository.getStorage().getId(), destRepository.getId(), blob);
                            log.info("destBlobPath {}", destBlobPath.toString());
                            artifactManagementService.store(destBlobPath, blobIs);
                        } catch (Exception e) {
                            log.error("{} blob copy error {}", relativizePath, ExceptionUtils.getStackTrace(e));
                        }
                    }
                    if (StringUtils.isNotBlank(manifest.getDigest())) {
                        //  copy manifest
                        String mainFestFileStr = arrayPath[0] + File.separator + "manifest" + File.separator + manifest.getDigest();
                        RepositoryPath srcMainFestPath = repositoryPathResolver.resolve(srcRepository.getStorage().getId(), srcRepository.getId(), mainFestFileStr);
                        try (InputStream inputStream = Files.newInputStream(srcMainFestPath)) {
                            RepositoryPath destManiFestPath = repositoryPathResolver.resolve(destRepository.getStorage().getId(), destRepository.getId(), mainFestFileStr);
                            artifactManagementService.store(destManiFestPath, inputStream);
                        } catch (Exception e) {
                            log.error("{} manifest copy error {}", relativizePath, ExceptionUtils.getStackTrace(e));
                        }
                    }
                }
            }
        }
        for (File file : list) {
            String fPath = file.getAbsolutePath();
            String tempStr = srcRepository.getStorage().getId() + File.separator + srcRepository.getId() + File.separator;
            int fPathIndex = fPath.lastIndexOf(tempStr);
            String temp = fPath.substring(fPathIndex).replace(tempStr, "");
            RepositoryPath destPath = repositoryPathResolver.resolve(destRepository.getStorage().getId(), destRepository.getId(), temp);
            if (RepositoryFiles.isChecksum(destPath) || RepositoryFiles.isArtifactMetadata(destPath)) {
                log.info(String.format("RepositoryPath：%s is checksum file skip", destPath));
                continue;
            }
            log.info("temp {}   destPath {}", temp, destPath.toString());
            boolean isDocker = DockerLayoutProvider.ALIAS.equalsIgnoreCase(srcRepository.getLayout());
            if (isDocker && !destPath.getFileName().toString().contains("sha256")) {
                log.info(String.format("RepositoryPath：%s not is docker layout file skip", destPath));
                continue;
            }
            try (InputStream is = Files.newInputStream(file.toPath());) {
                // 同步metadata
                RepositoryPath srcPath = repositoryPathResolver.resolve(srcRepository.getStorage().getId(), srcRepository.getId(), temp);
                setMetaData(destPath, getMetaData(srcPath));
                artifactManagementService.store(destPath, is);
            } catch (IOException e) {
                log.error("sync metaData error：{}", ExceptionUtils.getStackTrace(e));
                throw new Exception(e.getMessage());
            }
        }
    }

    public void handleS3ArtifactCopy(String path, Repository destRepository, Repository srcRepository) throws Exception {
        S3Path s3Path = new S3Path(SpringUtil.getBean(S3FileSystem.class), path);
        List<S3Path> s3FilesPaths = RepositoryPathUtil.getS3FiePaths(s3Path);
        if (DockerLayoutProvider.ALIAS.equalsIgnoreCase(srcRepository.getLayout())) {
            s3FilesPaths = sortS3Docker(s3FilesPaths, srcRepository.getLayout());
        }
        // 判断是否是docker 版本路径的复制
        boolean isDockerVersion = isDockerVersion(srcRepository.getLayout(), s3FilesPaths.stream().map(S3Path::toString).collect(Collectors.toList()));
        if (isDockerVersion) {
            // copy blobs manifest
            String tempStr = srcRepository.getStorage().getId() + File.separator + srcRepository.getId() + File.separator;
            int fPathIndex = path.lastIndexOf(tempStr);
            String relativizePath = path.substring(fPathIndex, path.length()).replace(tempStr, "");
            String[] arrayPath = relativizePath.split(File.separator);
            if (arrayPath.length != 2) {
                return;
            }
            List<S3Path> fileContents = s3FilesPaths.stream().filter(file -> DockerArtifactCoordinates.include(file.toAbsolutePath().toString())).collect(Collectors.toList());
            S3Path filePath = fileContents.get(0);
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(srcRepository.getStorage().getId(), srcRepository.getId(), String.format("%s/%s", relativizePath, filePath.getFileName().toString()));
            List<ImageManifest> imageManifestList = dockerComponent.getImageManifests(repositoryPath);
            if (CollectionUtils.isNotEmpty(imageManifestList)) {
                for (ImageManifest manifest : imageManifestList) {
                    List<String> layerList = getAllLayerList(manifest);
                    //blobs
                    for (String layer : layerList) {
                        String blob = arrayPath[0] + File.separator + "blobs" + File.separator + layer;
                        RepositoryPath vSrcBlobPath = repositoryPathResolver.resolve(srcRepository.getStorage().getId(), srcRepository.getId(), blob);
                        try (InputStream blobIs = Files.newInputStream(vSrcBlobPath)) {
                            RepositoryPath destBlobPath = repositoryPathResolver.resolve(destRepository.getStorage().getId(), destRepository.getId(), blob);
                            log.info("destBlobPath {}", destBlobPath.toString());
                            artifactManagementService.store(destBlobPath, blobIs);
                        } catch (Exception e) {
                            log.error("{} blob copy error {}", relativizePath, ExceptionUtils.getStackTrace(e));
                        }
                    }
                    if (StringUtils.isNotBlank(manifest.getDigest())) {
                        //manifest
                        String mainFestFileStr = arrayPath[0] + File.separator + "manifest" + File.separator + manifest.getDigest();
                        RepositoryPath srcMainFestPath = repositoryPathResolver.resolve(srcRepository.getStorage().getId(), srcRepository.getId(), mainFestFileStr);
                        try (InputStream inputStream = Files.newInputStream(srcMainFestPath)) {
                            RepositoryPath destManiFestPath = repositoryPathResolver.resolve(destRepository.getStorage().getId(), destRepository.getId(), mainFestFileStr);
                            artifactManagementService.store(destManiFestPath, inputStream);
                        } catch (Exception e) {
                            log.error("{} manifest copy error {}", relativizePath, ExceptionUtils.getStackTrace(e));
                        }
                    }
                }
            }
        }
        for (S3Path s3FilePath : s3FilesPaths) {
            log.info("s3FilePath {} copy start", s3FilePath);
            String fPath = s3FilePath.toString();
            String tempStr = srcRepository.getStorage().getId() + File.separator + srcRepository.getId() + File.separator;
            int fPathIndex = fPath.lastIndexOf(tempStr);
            String temp = fPath.substring(fPathIndex, fPath.length()).replace(tempStr, "");
            RepositoryPath destPath = repositoryPathResolver.resolve(destRepository.getStorage().getId(), destRepository.getId(), temp);
            if (RepositoryFiles.isChecksum(destPath) || RepositoryFiles.isArtifactMetadata(destPath)) {
                log.info(String.format("RepositoryPath：%s is checksum file skip", destPath));
                continue;
            }
            boolean isDocker = DockerLayoutProvider.ALIAS.equalsIgnoreCase(srcRepository.getLayout());
            if (isDocker && !destPath.getFileName().toString().contains("sha256")) {
                log.info(String.format("RepositoryPath：%s not is docker layout file skip", destPath));
                continue;
            }
            try (InputStream is = Files.newInputStream(s3FilePath);) {
                // 同步metadata
                RepositoryPath srcPath = repositoryPathResolver.resolve(srcRepository.getStorage().getId(), srcRepository.getId(), temp);
                setMetaData(destPath, getMetaData(srcPath));
                artifactManagementService.store(destPath, is);
            } catch (IOException e) {
                log.error("s3FilePath {} copy fail {}", s3FilePath, ExceptionUtils.getStackTrace(e));
                throw new Exception(e.getMessage());
            }
        }
        s3FilesPaths.clear();
    }

    public PromotionFileRelativePath getFileRelativePaths(RepositoryPath repositoryPath, boolean isDockerVersionPath) throws Exception {
        String repositoryId = repositoryPath.getRepository().getId();
        String storageId = repositoryPath.getRepository().getStorage().getId();
        String absolutePath = repositoryPath.toAbsolutePath().toString();
        List<String> list = new ArrayList<String>();
        Map<String, Object> metaData = new HashMap<>();
        boolean isDockerLayout = DockerLayoutProvider.ALIAS.equalsIgnoreCase(repositoryPath.getRepository().getLayout());
        if (absolutePath.contains("s3://")) {
            S3Path s3Path = new S3Path(SpringUtil.getBean(S3FileSystem.class), repositoryPath.getTarget().toString());
            List<S3Path> s3FilesPaths = RepositoryPathUtil.getS3FiePaths(s3Path);
            if (isDockerLayout) {
                s3FilesPaths = sortS3Docker(s3FilesPaths, repositoryPath.getRepository().getLayout());
            }
            if (isDockerVersionPath) {
                String tempStr = storageId + File.separator + repositoryId + File.separator;
                int fPathIndex = absolutePath.lastIndexOf(tempStr);
                String relativizePath = absolutePath.substring(fPathIndex, absolutePath.length()).replace(tempStr, "");
                String[] arrayPath = relativizePath.split(File.separator);
                List<ImageManifest> imageManifestList = dockerComponent.getImageManifests(repositoryPath);
                if (CollectionUtils.isNotEmpty(imageManifestList)) {
                    for (ImageManifest manifest : imageManifestList) {
                        //blobs
                        for (String layer : getAllLayerList(manifest)) {
                            String blob = arrayPath[0] + File.separator + "blobs" + File.separator + layer;
                            list.add(blob);
                        }
                        if (StringUtils.isNotBlank(manifest.getDigest())) {
                            String mainFestFile = arrayPath[0] + File.separator + "manifest" + File.separator + manifest.getDigest();
                            list.add(mainFestFile);
                        }
                    }
                }
            }
            for (S3Path file : s3FilesPaths) {
                String filePathStr = file.toAbsolutePath().toString();
                if (isDockerLayout && !file.getFileName().toString().contains("sha256")) {
                    log.info(String.format("RepositoryPath：%s not is docker layout file skip", filePathStr));
                    continue;
                }
                int indexTemp = filePathStr.indexOf(storageId + "/" + repositoryId);
                String temp = filePathStr.
                        substring(indexTemp + (storageId + "/" + repositoryId).length());
                if (temp.startsWith("/")) {
                    temp = temp.substring(1);
                }
                list.add(temp);
                // 添加跨节点元数据
                RepositoryPath srcRepositoryPath = repositoryPathResolver.resolve(repositoryPath.getRepository(), temp);
                metaData.put(temp, getMetaData(srcRepositoryPath));
            }
        } else {
            List<File> files = RepositoryPathUtil.getNFSFiles(absolutePath);
            if (isDockerLayout) {
                files = sortDocker(files, repositoryPath.getRepository().getLayout());
            }
            if (isDockerVersionPath) {
                String tempStr = storageId + File.separator + repositoryId + File.separator;
                int fPathIndex = absolutePath.lastIndexOf(tempStr);
                String relativizePath = absolutePath.substring(fPathIndex, absolutePath.length()).replace(tempStr, "");
                String[] arrayPath = relativizePath.split(File.separator);
                List<ImageManifest> imageManifestList = dockerComponent.getImageManifests(repositoryPath);
                if (CollectionUtils.isNotEmpty(imageManifestList)) {
                    for (ImageManifest manifest : imageManifestList) {
                        //blobs
                        for (String layer : getAllLayerList(manifest)) {
                            String blob = arrayPath[0] + File.separator + "blobs" + File.separator + layer;
                            list.add(blob);
                        }
                        //manifest
                        if (StringUtils.isNotBlank(manifest.getDigest())) {
                            String mainFestFile = arrayPath[0] + File.separator + "manifest" + File.separator + manifest.getDigest();
                            list.add(mainFestFile);
                        }
                    }
                }
            }
            for (File file : files) {
                String fileAbsolutePath = file.getAbsolutePath();
                if (isDockerLayout && !file.getName().contains("sha256")) {
                    log.info(String.format("RepositoryPath：%s not is docker layout file skip", fileAbsolutePath));
                    continue;
                }
                int indexTemp = fileAbsolutePath.indexOf(storageId + "/" + repositoryId);
                String temp = fileAbsolutePath.
                        substring(indexTemp + (storageId + "/" + repositoryId).length());
                if (temp.startsWith("/")) {
                    temp = temp.substring(1);
                }
                list.add(temp);
                // 添加跨节点元数据
                RepositoryPath srcRepositoryPath = repositoryPathResolver.resolve(repositoryPath.getRepository(), temp);
                metaData.put(temp, getMetaData(srcRepositoryPath));
            }
        }
        return new PromotionFileRelativePath(list, metaData);
    }


    public static String getBucket(String path) {
        return path.replace("s3://", "").split("/")[1];
    }

    public static String getS3Uri(String path) {
        String[] array = path.replace("s3://", "").split("/");
        StringBuilder stringBuilder = new StringBuilder();
        List<String> list = Arrays.asList(array);
        for (int i = 0; i < list.size(); i++) {
            if (i == 0) {
                continue;
            }
            if (i + 1 == list.size()) {
                stringBuilder.append(list.get(i));
            } else {
                stringBuilder.append(list.get(i)).append(File.separator);
            }

        }
        return stringBuilder.toString();
    }

    /**
     * 以post方式调用第三方接口,以form-data 形式  发送 MultipartFile 文件数据
     *
     * @param url       post请求url
     * @param uploadDto 晋级上传参数实体
     * @return string
     */
    public String upload(String url, PromotionNodeOptionDto uploadDto) throws Exception {
        Response response = null;
        try {
            if (Objects.isNull(uploadDto) || MapUtils.isEmpty(uploadDto.getPathMap())) {
                return "";
            }
            FormDataMultiPart part = new FormDataMultiPart();
            part.field("storageId", uploadDto.getStorageId());
            part.field("repostoryId", uploadDto.getRepostoryId());

            HashMap<String, String> filePathMap = new HashMap<String, String>();
            uploadDto.getPathMap().forEach((x, y) -> {
                y.forEach((j, z) -> {
                    part.bodyPart(new StreamDataBodyPart("files", z, j));
                    filePathMap.put(j, x);
                });
            });
            part.field("filePathMap", JSON.toJSONString(filePathMap));
            part.field("fileMetaDataMap", JSON.toJSONString(uploadDto.getFileMetaDataMap()));
            part.field("promotion", "true");
            Client client = clientPool.getRestClient();
            WebTarget resource = client.register(MultiPartWriter.class).target(url);
            Invocation.Builder builder = resource.request(MediaType.APPLICATION_JSON);
            securityComponent.securityTokenHeader(builder);
            response = builder.header("Mime-Version", "1.0").
                    post(Entity.entity(part, Boundary.addBoundary(MediaType.MULTIPART_FORM_DATA_TYPE)));
            if (response.getStatus() != 200) {
                log.info("upload failed [{}]", response.readEntity(String.class));
                throw new Exception("upload failed ");
            }
            response.readEntity(String.class);
        } catch (Exception e) {
            log.error("推送制品失败：{}", ExceptionUtils.getStackTrace(e));
            throw new Exception(e.getMessage());
        } finally {
            if (Objects.nonNull(response)) {
                response.close();
            }
            uploadDto.getPathMap().forEach((x, y) -> {
                y.forEach((j, z) -> {
                    if (null != z) {
                        try {
                            z.close();
                        } catch (IOException e) {
                            e.printStackTrace();
                        }

                    }
                });
            });
        }
        return "上传成功";
    }

    public String getMetaData(RepositoryPath srcPath) {
        String rs = "";
        try {
            if (Objects.isNull(srcPath) || !Files.exists(srcPath)) {
                return rs;
            }
            Artifact artifact = artifactWebService.getArtifact(srcPath);
            if (Objects.isNull(artifact)) {
                return rs;
            }
            rs = artifact.getMetadata();
        } catch (Exception e) {
            log.error("Exception {}", ExceptionUtils.getStackTrace(e));
        }
        return rs;
    }

    /**
     * 处理metadata
     *
     * @param repositoryPath repositoryPath
     * @param metadata       metadata
     */
    public void setMetaData(RepositoryPath repositoryPath, String metadata) {
        if (Objects.nonNull(repositoryPath) && StringUtils.isNotBlank(metadata) && JSONUtil.isJson(metadata)) {
            try {
                Artifact artifact = Optional.ofNullable(repositoryPath.getArtifactEntry())
                        .orElse(new ArtifactEntity(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(),
                                RepositoryFiles.readCoordinates(repositoryPath)));
                artifact.setMetadata(metadata);
                repositoryPath.setArtifact(artifact);
            } catch (Exception ex) {
                log.error("setMetaData Exception {} repositoryPath {} metadata {}", ExceptionUtils.getStackTrace(ex), repositoryPath.toString(), metadata);
            }
        }
    }

    /**
     * 校验是否是对docker版本的操作
     *
     * @param layout    布局类型
     * @param fileNames 文件名
     * @return true 是 false 不是
     */
    public boolean isDockerVersion(String layout, List<String> fileNames) {
        return DockerLayoutProvider.ALIAS.equalsIgnoreCase(layout) && fileNames.stream().allMatch(item -> !item.contains("blobs/sha256") && !item.contains("manifest/sha256"));
    }

    /**
     * 校验是否是对docker版本的操作
     *
     * @param layout 布局类型
     * @param path   文件名
     * @return true 是 false 不是
     */
    public boolean isDockerVersion(String layout, String path) {
        String split = "/";
        return DockerLayoutProvider.ALIAS.equalsIgnoreCase(layout) && path.split(split).length == 2 && !path.contains("blobs/sha256") && !path.contains("manifest/sha256");
    }

    private List<File> sortDocker(List<File> list, String layout) {
        if (CollectionUtils.isNotEmpty(list) && DockerLayoutProvider.ALIAS.equalsIgnoreCase(layout)) {
            String blobs = "blobs", manifest = "manifest";
            if (CollectionUtils.isNotEmpty(list)) {
                List<File> blobsFileList = list.stream().filter(item -> item.getParent().contains(blobs)).collect(Collectors.toList());
                List<File> manifestFileList = list.stream().filter(item -> item.getParent().contains(manifest)).collect(Collectors.toList());
                List<File> otherFileList = list.stream().filter(item -> blobsFileList.stream().noneMatch(blobItem -> blobItem.getAbsolutePath().equals(item.getAbsolutePath())) && manifestFileList.stream().noneMatch(manifestItem -> manifestItem.getAbsolutePath().equals(item.getAbsolutePath()))).collect(Collectors.toList());
                list = Lists.newArrayList();
                list.addAll(blobsFileList);
                list.addAll(manifestFileList);
                list.addAll(otherFileList);
            }
        }
        return list;
    }

    private List<S3Path> sortS3Docker(List<S3Path> s3FilesPaths, String layout) {
        String blobs = "blobs", manifest = "manifest";
        if (CollectionUtils.isNotEmpty(s3FilesPaths) && DockerLayoutProvider.ALIAS.equalsIgnoreCase(layout)) {
            List<S3Path> blobsFileList = s3FilesPaths.stream().filter(item -> item.getParent().toString().contains(blobs)).collect(Collectors.toList());
            List<S3Path> manifestFileList = s3FilesPaths.stream().filter(item -> item.getParent().toString().contains(manifest)).collect(Collectors.toList());
            List<S3Path> otherFileList = s3FilesPaths.stream().filter(item -> blobsFileList.stream().noneMatch(blobItem -> blobItem.toString().equals(item.toString())) && manifestFileList.stream().noneMatch(manifestItem -> manifestItem.toString().equals(item.toString()))).collect(Collectors.toList());
            s3FilesPaths = Lists.newArrayList();
            s3FilesPaths.addAll(blobsFileList);
            s3FilesPaths.addAll(manifestFileList);
            s3FilesPaths.addAll(otherFileList);
        }
        return s3FilesPaths;
    }

    private List<String> getAllLayerList(ImageManifest imageManifest) {
        if (Objects.nonNull(imageManifest) && CollectionUtils.isNotEmpty(imageManifest.getLayers())) {
            List<String> layerList = imageManifest.getLayers().stream().map(LayerManifest::getDigest).collect(Collectors.toList());
            if (Objects.nonNull(imageManifest.getConfig())) {
                layerList.add(imageManifest.getConfig().getDigest());
            }
            return layerList;
        }
        return Collections.emptyList();
    }
    
    public List<ArtifactSliceUploadHttpEntityResponse> artifactSliceUpload(PromotionNodeOptionDto uploadDto, String targetUrl, String storageId, String repositoryId, String syncNo) {
        targetUrl = StringUtils.chomp(targetUrl, "/");
        final Map<String, Map<String, Path>> filePathMap = uploadDto.getFilePathMap();
        final long sliceByteSize = Optional.ofNullable(configurationManagementService.getConfiguration().getSliceMbSize()).orElse(0L) * (1024 * 1024);
        final HttpClient httpClient = HttpClients.createDefault();
        final HttpPost httpPost = new HttpPost(String.format("%s/api/artifact/folib/promotion/slice/upload", targetUrl));
        final List<PromotionUtil.ArtifactSliceUploadHttpEntityBuilder> artifactSliceUploadHttpEntityList = this.getArtifactSliceUploadHttpEntityList(filePathMap, storageId, repositoryId, sliceByteSize);

        // 记录制品从记录
        String finalTargetUrl = targetUrl;
        artifactSliceUploadHttpEntityList.stream().forEach(e -> {
            final ArtifactSyncSlaveRecord artifactSyncSlaveRecord = new ArtifactSyncSlaveRecord();
            artifactSyncSlaveRecord.setSourcePath(e.getPath());
            artifactSyncSlaveRecord.setTargetPath(String.format("%s/%s/%s/%s-chunk%s", finalTargetUrl, e.getStorageId(), e.getRepositoryId(), e.getPath(), e.getChunkIndex()));
            artifactSyncSlaveRecord.setSyncNo(syncNo);
            artifactSyncSlaveRecord.setSyncModel(ArtifactSyncRecordSyncModelEnum.PUSH.getVal());
            artifactSyncSlaveRecord.setStatus(ArtifactSyncRecordStatusEnum.IN_SYNC.getVal());
//            artifactSyncSlaveRecord.setCreateBy(userName);
            artifactSyncSlaveRecord.setCreateTime(new Date());

            artifactSyncSlaveRecordMapper.insert(artifactSyncSlaveRecord);
            e.setChunkArtifactRecordId(artifactSyncSlaveRecord.getId());
        });

        return artifactSliceUploadHttpEntityList.stream().map(builder -> {
            httpPost.reset();
            httpPost.setEntity(builder.build());
            final ArtifactSliceUploadHttpEntityResponse res = new ArtifactSliceUploadHttpEntityResponse();
            res.setChunkArtifactRecordId(builder.getChunkArtifactRecordId());
            res.setSuccess(false);
            try {
                final HttpResponse response = httpClient.execute(httpPost);
                int responseCode = response.getStatusLine().getStatusCode();

                res.setSuccess(HttpStatus.OK.value() == responseCode);
                if (!res.getSuccess()) {
                    res.setFailedReason(String.format("上传制品(%s)切片失败", builder.getPath()));
                }
            } catch (IOException e) {
                res.setFailedReason(e.getMessage());
                log.error("制品切片上传失败", e);
            }
            
            // 更新记录状态
            artifactSyncSlaveRecordMapper.updateRecordStatus(builder.getChunkArtifactRecordId(), res.getSuccess() ? ArtifactSyncRecordStatusEnum.SUCCESS.getVal():ArtifactSyncRecordStatusEnum.FAILED.getVal(), new Date(), res.getFailedReason());
            return res;
        }).collect(Collectors.toList());
    }

    private List<ArtifactSliceUploadHttpEntityBuilder> getArtifactSliceUploadHttpEntityList(Map<String, Map<String, Path>> filePathMap, String storageId, String repositoryId, long chunkSize) {
        return filePathMap.values().stream().map(m -> {
            return m.entrySet().stream().map(entry -> {
                final String saveUri = entry.getKey();
                final Path path = entry.getValue();
                return this.getArtifactSliceUploadHttpEntityList(storageId, repositoryId, saveUri, path, chunkSize);
            }).flatMap(Collection::stream).collect(Collectors.toList());
        }).flatMap(Collection::stream).collect(Collectors.toList());
    } 
    
    
    private List<ArtifactSliceUploadHttpEntityBuilder> getArtifactSliceUploadHttpEntityList(String storageId, String repositoryId, String saveUri, Path artifactPath, long chunkSize) {
        try {
            final long fileLength = Files.size(artifactPath);
            final int threadCount = BigDecimal.valueOf(fileLength).divide(BigDecimal.valueOf(chunkSize), 0, RoundingMode.CEILING).intValue();
            final String md5 = FileUtils.getMD5(Files.newInputStream(artifactPath));
            final String mergeId = UUID.randomUUID().toString(true);

            return IntStream.range(0, threadCount).mapToObj(index -> {
                long startLength = index * chunkSize;
                try {

                    return new ArtifactSliceUploadHttpEntityBuilder()
                    .setStorageId(storageId)
                    .setRepositoryId(repositoryId)
                    .setPath(saveUri)
                    .setMergeId(mergeId)
                    .setChunkIndex(index + 1)
                    .setChunkIndexMax(threadCount)
                    .setOriginFileMd5(md5)
                    .setArtifactPath(artifactPath)
                    .setStartLength(startLength)
                    .setChunkSize(chunkSize);
                } catch (Exception e) {
                    log.error("构建文件切片HttpEntity请求失败", e);
                    return null;
                }
            }).collect(Collectors.toList());
        }catch (Exception e) {
            log.error("构建文件切片请求集合失败", e);
            return Collections.emptyList();
        }
    }

    
    @Data
    @Accessors(chain = true)
    public static class ArtifactSliceUploadHttpEntityBuilder {
        /** 制品切片记录ID */
        private Long chunkArtifactRecordId;
        private String storageId;
        private String repositoryId;
        private String path;
        private String mergeId;
        private Integer chunkIndex;
        private Integer chunkIndexMax;
        private String originFileMd5;
        private Path artifactPath;
        private Long startLength;
        private Long chunkSize;
        
        public HttpEntity build() {
            try {
                return MultipartEntityBuilder.create()
                        .setContentType(ContentType.MULTIPART_FORM_DATA)
                        .addPart("storageId", new StringBody(storageId))
                        .addPart("repositoryId", new StringBody(repositoryId))
                        .addPart("path", new StringBody(path))
                        .addPart("mergeId", new StringBody(mergeId))
                        .addPart("chunkIndex", new StringBody(String.valueOf(chunkIndex)))
                        .addPart("chunkIndexMax", new StringBody(String.valueOf(chunkIndexMax)))
                        .addPart("originFileMd5", new StringBody(originFileMd5))
                        .addPart("file", new InputStreamBody(new BufferedInputStreamWrapper(Files.newInputStream(artifactPath), startLength, chunkSize), "chunk" + chunkIndex))
                        .build();
            } catch (Exception e) {
                log.error("构建文件切片HttpEntity请求失败", e);
                return null;
            }
        }
    }

    @Data
    @Accessors(chain = true)
    public static class ArtifactSliceUploadHttpEntityResponse {
        /** 制品切片记录ID */
        private Long chunkArtifactRecordId;
        private Boolean success;
        private String failedReason;
    }
}
