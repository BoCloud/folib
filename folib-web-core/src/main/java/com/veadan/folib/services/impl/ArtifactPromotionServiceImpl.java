package com.veadan.folib.services.impl;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.io.FileUtil;
import cn.hutool.core.io.IoUtil;
import cn.hutool.core.lang.UUID;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.cloud.storage.s3fs.util.UriUtils;
import com.veadan.folib.components.IdGenerateUtils;
import com.veadan.folib.components.artifact.ArtifactComponent;
import com.veadan.folib.components.promotion.ArtifactPromotionProvider;
import com.veadan.folib.components.promotion.ArtifactPromotionProviderRegistry;
import com.veadan.folib.components.security.SecurityComponent;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.controllers.promotion.ArtifactPromotionController;
import com.veadan.folib.dispatch.ClusterDispatchNodeDto;
import com.veadan.folib.domain.*;
import com.veadan.folib.dto.*;
import com.veadan.folib.entity.ArtifactSyncRecord;
import com.veadan.folib.entity.Dict;
import com.veadan.folib.enums.ArtifactSyncRecordStatusEnum;
import com.veadan.folib.enums.ArtifactSyncRecordSyncModelEnum;
import com.veadan.folib.mapper.ArtifactSyncRecordMapper;
import com.veadan.folib.model.request.ArtifactSliceDownloadInfoReq;
import com.veadan.folib.model.request.ArtifactSupportSliceDownloadQueryReq;
import com.veadan.folib.model.response.ArtifactSliceDownloadInfoRes;
import com.veadan.folib.promotion.ArtifactUploadTask;
import com.veadan.folib.promotion.PromotionUtil;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.layout.LayoutProviderRegistry;
import com.veadan.folib.providers.storage.S3FileSystemStorageProvider;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.repository.MavenRepositoryFeatures;
import com.veadan.folib.scanner.common.exception.BusinessException;
import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.veadan.folib.services.*;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import com.veadan.folib.users.userdetails.SpringSecurityUser;
import com.veadan.folib.util.SocketUtils;
import com.veadan.folib.utils.FileUtils;
import com.veadan.folib.utils.PropertiesUtils;
import com.veadan.folib.utils.UrlUtils;
import com.veadan.folib.ws.client.handler.command.FolibWsClientArtifactPullCommand;
import com.veadan.folib.ws.common.FolibWsAction;
import com.veadan.folib.ws.server.manage.FolibWsServerRunManage;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.fileupload.disk.DiskFileItem;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.maven.model.Model;
import org.glassfish.jersey.client.ClientProperties;
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
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.multipart.commons.CommonsMultipartFile;
import software.amazon.awssdk.utils.StringInputStream;

import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.Invocation;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.Response;
import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.FutureTask;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

import static com.veadan.folib.utils.UrlUtils.parsePath;

/**
 * @author qijianping
 */
@Service
@Slf4j
public class ArtifactPromotionServiceImpl implements ArtifactPromotionService {

    private final String upLoadURI = "/api/artifact/folib/promotion/upload-files";
    private final String REPOSITORY_URL = "api/configuration/folib/storages";

    @Inject
    private RepositoryManagementService repositoryManagementService;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private ArtifactManagementService artifactManagementService;

    @Autowired
    private PromotionUtil promotionUtil;

    @Autowired
    private ThreadPoolTaskExecutor asyncThreadPoolTaskExecutor;

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
        promotionUtil.executeCopy(srcPath, srcRepository, destRepository);
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
    public ResponseEntity nodeOption(PromotionNodeOption promotionNodeOption, HttpServletRequest request) {
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
                promotionUtil.executeCopy(srcPath, srcRepository, destRepository);
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

                //向目标仓库推包
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
            }
        } catch (Exception e) {
            log.error("制品晋级错误 {}", ExceptionUtils.getStackTrace(e));
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(e.getMessage());
        }
        return ResponseEntity.ok("ok");
    }

    @Override
    public ResponseEntity nodeOptionAttachRecord(PromotionNodeOption promotionNodeOption, HttpServletRequest
            request) {
        PromotionRepositoryInfo promotionRepositoryInfo = resolvePromotionRepository(promotionNodeOption);
        if (Objects.isNull(promotionNodeOption.getSyncModel())) {
            promotionNodeOption.setSyncModel(ArtifactSyncRecordSyncModelEnum.PUSH.getVal());
        }
        if (ArtifactSyncRecordSyncModelEnum.PUSH.getVal().equals(promotionNodeOption.getSyncModel())) {
            validateSourceRepositoryPath(promotionRepositoryInfo.getSourceStorageId(), promotionRepositoryInfo.getSourceRepositoryId(), promotionRepositoryInfo.getSourceArtifactPath());
            validateRemoteRepository(promotionRepositoryInfo.getTargetBaseUrl(), promotionRepositoryInfo.getTargetStorageId(), promotionRepositoryInfo.getTargetRepositoryId());
        }
        // 生成同步编号
        final String syncNo = String.format("SyncNo-%s", UUID.fastUUID());
        final SpringSecurityUser userDetails = (SpringSecurityUser) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        final String userName = Optional.ofNullable(userDetails).map(SpringSecurityUser::getUsername).orElse(null);

        // 生成日志记录
        final ArtifactSyncRecord artifactSyncRecord = new ArtifactSyncRecord();
        artifactSyncRecord.setId(idGenerateUtils.generateId("artifactSyncRecordId"));
        artifactSyncRecord.setSourcePath(promotionNodeOption.getSourcePath());
        artifactSyncRecord.setTargetPath(promotionNodeOption.getTargetPath());
        artifactSyncRecord.setSyncNo(syncNo);
        artifactSyncRecord.setOpsType(1);
        artifactSyncRecord.setSyncModel(promotionNodeOption.getSyncModel());
        artifactSyncRecord.setStatus(ArtifactSyncRecordStatusEnum.IN_SYNC.getVal());
        artifactSyncRecord.setCreateBy(userName);
        artifactSyncRecord.setCreateTime(new Date());
        artifactSyncRecordMapper.insert(artifactSyncRecord);

        try {
            asyncPromotionPoolTaskExecutor.execute(() ->
            { // 异步执行制品晋级
                ResponseEntity re = this.nodeOption(promotionNodeOption, request);
                if (HttpStatus.OK.equals(re.getStatusCode())) {
                    artifactSyncRecord.setStatus(ArtifactSyncRecordStatusEnum.SUCCESS.getVal());
                } else {
                    artifactSyncRecord.setStatus(ArtifactSyncRecordStatusEnum.FAILED.getVal());
                    if (Objects.nonNull(re.getBody())) {
                        artifactSyncRecord.setFailedReason(re.getBody().toString());
                    }
                }

                // 更新日志结束开始时间
                artifactSyncRecordMapper.updateByPrimaryKey(artifactSyncRecord
                        .setUpdateTime(new Date())
                        .setUpdateBy(userName));
            });
        } catch (Exception e) {
            artifactSyncRecord.setStatus(ArtifactSyncRecordStatusEnum.FAILED.getVal());
            artifactSyncRecord.setFailedReason(e.getMessage());

            // 更新日志结束开始时间
            artifactSyncRecordMapper.updateByPrimaryKey(artifactSyncRecord
                    .setUpdateTime(new Date())
                    .setUpdateBy(userName));
        }

        return ResponseEntity.ok(syncNo);
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
            filePathMap, String fileMetaDataMap, String uuid) {
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
                ArtifactUploadTask artifactUploadTask = new ArtifactUploadTask(storageId, repositoryId, file,
                        repositoryManagementService, repositoryPathResolver, artifactManagementService, promotionUtil, layoutProviderRegistry, artifactMetadataService, artifactRepository, mavenRepositoryFeatures, tempPath, fileRelativePath, metaData, uuid, null);
                FutureTask<String> task = new FutureTask<String>(artifactUploadTask);
                listTask.add(task);
                asyncThreadPoolTaskExecutor.submit(task);
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
            asyncThreadPoolTaskExecutor.submit(futureTask);
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
    public ResponseEntity artifactDispatch(ArtifactDispatch artifactDispatch) {
        log.info("start artifact dispatch");
        try {
            artifactDispatch.setPath(UriUtils.decode(artifactDispatch.getPath()));
        } catch (Exception ex) {
            log.warn(ExceptionUtils.getStackTrace(ex));
        }
        Map<String, List<TargetDispatchRepositoryDto>> groupByMap = artifactDispatch.getTargetDispatchRepositoryList().stream().collect(Collectors.groupingBy(TargetDispatchRepositoryDto::getArtifactoryRepositoryType));
        for (Map.Entry<String, List<TargetDispatchRepositoryDto>> item : groupByMap.entrySet()) {
            ArtifactPromotionProvider artifactPromotionProvider = artifactPromotionProviderRegistry.getProvider(item.getKey());
            ArtifactDispatch itemArtifactDispatch = new ArtifactDispatch();
            BeanUtils.copyProperties(artifactDispatch, itemArtifactDispatch);
            itemArtifactDispatch.setTargetDispatchRepositoryList(item.getValue());
            artifactPromotionProvider.dispatch(itemArtifactDispatch);
        }
        return ResponseEntity.ok("ok");
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
    public void validateRemoteRepository(String targetUrl, String storageId, String repositoryId) {
        targetUrl = String.format("%s/%s/%s/%s", StringUtils.removeEnd(targetUrl, GlobalConstants.SEPARATOR), REPOSITORY_URL, storageId, repositoryId);
        Response response = null;
        try {
            Client client = clientPool.getRestClient();
            //连接建立超时时间
            client.property(ClientProperties.CONNECT_TIMEOUT, 5000);
            //读取内容超时时间
            client.property(ClientProperties.READ_TIMEOUT, 5000);
            WebTarget target = client.target(targetUrl);
            Invocation.Builder builder = target.request();
            securityComponent.securityTokenHeader(builder);
            response = builder.head();
            if (HttpStatus.OK.value() != response.getStatus()) {
                throw new BusinessException(String.format("Remote repository [%s] [%s]  not exist!", storageId, repositoryId));
            }
        } catch (Exception ex) {
            log.error("Validate remote repository [{}] [{}] [{}] error [{}]", targetUrl, storageId, repositoryId, ExceptionUtils.getStackTrace(ex));
            throw new BusinessException(ex.getMessage());
        } finally {
            if (Objects.nonNull(response)) {
                response.close();
            }
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
    public Boolean sliceFileDownload(Repository repository, String artifactPath, String
            nodeMark, HttpServletResponse response) {
        // 获取全局节点限速
        final int kbps = Optional.ofNullable(configurationManagementService.getConfiguration().getKbps()).orElse(0) * (1024);
        // 获取节点限速
        final Collection<ClusterDispatchNodeDto> clusterDispatchNodeDtos = configurationManagementService.getMutableConfigurationClone().getClusterDispatchNode().values();
        final Map<String, Integer> nodeKbpsMap = clusterDispatchNodeDtos.stream().filter(e -> null == e.getAutoRegister() || !e.getAutoRegister()).collect(Collectors.toMap(e -> String.format("%s:%s", UrlUtils.getHost(e.getClusterNodeHost()), UrlUtils.getPort(e.getClusterNodeHost())), e -> null != e.getKbps() ? e.getKbps() * 1024 : 0));
        final int finalKbps = Optional.ofNullable(nodeKbpsMap.get(nodeMark)).filter(k -> k > 0).orElse(kbps);

        // 下载文件流
        final RepositoryPath artifactRepositoryPath = repositoryPathResolver.resolve(repository, artifactPath);
        final String fileName = artifactRepositoryPath.getFileName().toString();
        response.setHeader("Content-Disposition", String.format("attachment;filename=%s", fileName));
        response.setContentType("application/x-gzip");
        Path slicePath;
        if (Files.exists(artifactRepositoryPath)) {
            // Folib
            slicePath = artifactRepositoryPath;
        } else {
            // Local-Temp（Slice file）
            final String storageId = repository.getStorage().getId();
            final String repositoryId = repository.getId();
            final String artifactFileSliceFilePath = String.format("%s/artifactSlice/%s/%s/%s", StringUtils.chomp(tempPath, "/"), storageId, repositoryId, artifactPath);
            final Path filePath = Path.of(artifactFileSliceFilePath);

            if (!Files.exists(filePath)) {
                throw new BusinessException("下载的切片文件不存在或还未生成");
            }
            slicePath = filePath;
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

            try (InputStream sliceFileInputSteam = Files.newInputStream(slicePath)) {
                this.sliceSpeedLimitDownload(sliceFileInputSteam, response.getOutputStream(), nodeDownloadConnectionCounter, finalKbps);
            } catch (Exception e) {
                log.error("限速下载文件失败", e);
                return false;
            } finally {
                nodeDownloadConnectionCounter.decrementAndGet();
            }
        } else {
            // 非限速下载
            try (final InputStream inputStream = Files.newInputStream(slicePath);
                 final OutputStream outputStream = response.getOutputStream();) {
                IoUtil.copy(inputStream, outputStream);
            } catch (Exception e) {
                log.error("非限速下载文件失败", e);
                return false;
            }
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

    @Override
    public Boolean querySupportSliceDownload(ArtifactSupportSliceDownloadQueryReq model) {
        final String storageId = model.getStorageId();
        final String repositoryId = model.getRepositoryId();
        final String path = model.getPath();
        final RepositoryPath artifactPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
        if (!Files.exists(artifactPath)) {
            throw new BusinessException("需要获取切片下载信息的制品不存在或已被删除");
        }

        try {
            final long artifactFileLength = Files.size(artifactPath);
            final long kbps = Optional.ofNullable(configurationManagementService.getConfiguration().getSliceMbSize()).orElse(0L) * (1024 * 1024);
            return artifactFileLength > kbps;
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException(ex);
        }
    }

    @Override
    public Map<String, Boolean> batchQuerySupportSliceDownload
            (List<ArtifactSupportSliceDownloadQueryReq> models) {
        final Map<String, Boolean> resultMap = new HashMap<>();
        for (ArtifactSupportSliceDownloadQueryReq model : models) {
            final String storageId = model.getStorageId();
            final String repositoryId = model.getRepositoryId();
            final String path = model.getPath();
            final String fullPath = String.format("%s/%s/%s", storageId, repositoryId, path);
            resultMap.put(fullPath, this.querySupportSliceDownload(model));
        }

        return resultMap;
    }

    @Override
    public ArtifactSliceDownloadInfoRes querySliceDownloadInfoStoreTemp(ArtifactSliceDownloadInfoReq model) {
        final String storageId = model.getStorageId();
        final String repositoryId = model.getRepositoryId();
        final String path = model.getPath();
        final ArtifactSliceDownloadInfoRes artifactSliceDownloadInfoDto = new ArtifactSliceDownloadInfoRes();
        final RepositoryPath artifactPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
        if (!Files.exists(artifactPath)) {
            throw new BusinessException("需要获取切片下载信息的制品不存在或已被删除");
        }
        if (Files.isDirectory(artifactPath)) {
            return null;
        }

        try {
            final Repository repository = artifactPath.getRepository();
            final Path fileName = artifactPath.getTarget().getFileName();
            final String baseUrl = StringUtils.chomp(configurationManagementService.getConfiguration().getBaseUrl(), "/");
            final String md5 = null != artifactPath.getArtifactEntry() ? Optional.ofNullable(artifactPath.getArtifactEntry().getChecksums()).orElse(Collections.emptyMap()).get("MD5") : null;
            final long kbps = Optional.ofNullable(configurationManagementService.getConfiguration().getSliceMbSize()).orElse(0L) * (1024 * 1024);
            if (kbps <= 0) {
                throw new BusinessException("制品传输切片大小不能为空，请前往全局配置进行配置");
            }

            final long artifactFileLength = Files.size(artifactPath);
            String artifactFilePath = artifactPath.toString();
            final String artifactParentUri = Optional.of(artifactPath.relativize()).map(p -> {
                try {
                    return p.getParent().toString();
                } catch (Exception e) {
                    return StringUtils.EMPTY;
                }
            }).get();

            artifactSliceDownloadInfoDto.setStorageId(storageId);
            artifactSliceDownloadInfoDto.setRepositoryId(repositoryId);
            artifactSliceDownloadInfoDto.setPath(path);
            artifactSliceDownloadInfoDto.setUsedSlice(artifactFileLength > kbps);
            artifactSliceDownloadInfoDto.setArtifactMd5(md5);

            if (artifactSliceDownloadInfoDto.getUsedSlice()) {
                try {
                    final String sliceStoreFolderUri = String.format("%s.slice", StringUtils.isNotBlank(artifactParentUri) ? artifactParentUri + "/" : StringUtils.EMPTY);
///                    final String sliceGenJsonFileUri = String.format("%s/slice-gen.json", sliceStoreFolderUri);
                    final String artifactFileSliceRootFolderPathStr = String.format("%s/artifactSlice/%s/%s", StringUtils.chomp(tempPath, "/"), storageId, repositoryId);
                    final String artifactFileSliceFolderPathStr = String.format("%s/%s", artifactFileSliceRootFolderPathStr, sliceStoreFolderUri);
///                    final String sliceGenJsonFilePathStr = String.format("%s/%s", artifactFileSliceRootFolderPathStr, sliceGenJsonFileUri);

                    // 根据文件MD5检查是否已经生成切片数据，如有则返回生成已经存在的切片数据（避免重复生成）
///                    final Path sliceGenJsonFilePath = Path.of(sliceGenJsonFilePathStr);
///                    if (Files.exists(sliceGenJsonFilePath)) {
///                        final String sliceGenJson = IoUtil.readUtf8(Files.newInputStream(sliceGenJsonFilePath));
///                        if (StringUtils.isNotBlank(sliceGenJson)) {
///                            final ArtifactSliceDownloadInfoRes cacheDto = JSON.parseObject(sliceGenJson, ArtifactSliceDownloadInfoRes.class);
///                            if (null != cacheDto && StringUtils.isNotBlank(md5) && md5.equals(cacheDto.getArtifactMd5())) {
///                                if (CollUtil.isNotEmpty(cacheDto.getDownloadPartList())) {
///                                    for (ArtifactSliceDownloadInfoRes.DownloadPartInfo downloadPartInfo : cacheDto.getDownloadPartList()) {
///                                        /** {@linkplain ArtifactPromotionController#speedLimitDownload(Repository, String, String, HttpServletResponse)} */
///                                        downloadPartInfo.setDownloadUrl(String.format("%s/artifactSlice/%s/%s/%s", baseUrl, storageId, repositoryId, downloadPartInfo.getDownloadUri()));
///                                    }
///                                }
///                                return cacheDto;
///                            }
///                        }
///                    }

                    if (S3FileSystemStorageProvider.ALIAS.equals(repository.getStorageProvider())) {
                        // 由于是网络路径，需要暂存到本地进行暂存
                        artifactFilePath = String.format("%s/artifactTemp/%s/%s", StringUtils.chomp(tempPath, "/"), UUID.randomUUID().toString(true), fileName);
                        try (InputStream inputStream = new BufferedInputStream(Files.newInputStream(artifactPath))) {
                            FileUtil.writeFromStream(inputStream, artifactFilePath);
                        }
                    }
                    final List<String> splitFilePathList = FileUtils.splitFile(artifactFilePath, artifactFileSliceFolderPathStr, kbps);

                    // 生成下载路径
                    final List<ArtifactSliceDownloadInfoRes.DownloadPartInfo> downloadPartInfoList = splitFilePathList.stream()
                            .map(splitFilePath -> {
                                final String splitFileName = FileUtil.getName(splitFilePath);
                                final String splitFileStoreUri = String.format("%s/%s", sliceStoreFolderUri, splitFileName);
                                return new ArtifactSliceDownloadInfoRes.DownloadPartInfo()
                                        .setDownloadUri(splitFileStoreUri)
                                        /** {@linkplain ArtifactPromotionController#speedLimitDownload(Repository, String, String, HttpServletResponse)} */
                                        .setDownloadUrl(String.format("%s/api/artifact/folib/promotion/file/speedLimitDownload/%s/%s/%s", baseUrl, storageId, repositoryId, splitFileStoreUri));
                            })
                            .collect(Collectors.toList());
                    artifactSliceDownloadInfoDto.setDownloadPartList(downloadPartInfoList);

                    // 持久化切片数据
///                    if (!Files.exists(sliceGenJsonFilePath)) {
///                        FileUtil.touch(sliceGenJsonFilePath.toFile());
///                    }
///                    Files.write(sliceGenJsonFilePath, JSON.toJSONString(artifactSliceDownloadInfoDto).getBytes(StandardCharsets.UTF_8));
                } catch (IOException e) {
                    log.error("切片制品文件失败", e);
                    throw new BusinessException("切片制品文件失败");
                }
            } else {
                final String artifactUri = String.format("%s/%s/%s", storageId, repositoryId, artifactPath.relativize());
                artifactSliceDownloadInfoDto.setDownloadPartList(Collections.singletonList(
                        new ArtifactSliceDownloadInfoRes.DownloadPartInfo()
                                .setDownloadUri(artifactUri)
                                .setDownloadUrl(String.format("%s/api/artifact/folib/promotion/file/speedLimitDownload/%s", baseUrl, artifactUri))
                ));
            }
        } catch (BusinessException e) {
            throw e;
        } catch (Exception e) {
            log.error("获取制品切片下载信息失败", e);
            throw new BusinessException("获取制品切片下载信息失败");
        }

        return artifactSliceDownloadInfoDto;
    }

    @Override
    public ArtifactSliceDownloadInfoRes querySliceDownloadInfoStoreFolib(ArtifactSliceDownloadInfoReq model) {
        final String storageId = model.getStorageId();
        final String repositoryId = model.getRepositoryId();
        final String path = model.getPath();
        final ArtifactSliceDownloadInfoRes artifactSliceDownloadInfoDto = new ArtifactSliceDownloadInfoRes();
        final RepositoryPath artifactPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
        if (!Files.exists(artifactPath)) {
            throw new BusinessException("需要获取切片下载信息的制品不存在或已被删除");
        }
        if (Files.isDirectory(artifactPath)) {
            return null;
        }

        try {
            final Repository repository = artifactPath.getRepository();
            final Path fileName = artifactPath.getTarget().getFileName();
            final String baseUrl = StringUtils.chomp(configurationManagementService.getConfiguration().getBaseUrl(), "/");
            final String md5 = null != artifactPath.getArtifactEntry() ? Optional.ofNullable(artifactPath.getArtifactEntry().getChecksums()).orElse(Collections.emptyMap()).get("MD5") : null;
            final long kbps = Optional.ofNullable(configurationManagementService.getConfiguration().getSliceMbSize()).orElse(0L) * (1024 * 1024);
            final long artifactFileLength = Files.size(artifactPath);
            String artifactFilePath = artifactPath.toString();
            String artifactFileSliceFolderPath = String.format("%s/artifactSlice/%s", StringUtils.chomp(tempPath, "/"), UUID.fastUUID().toString(true));
            artifactSliceDownloadInfoDto.setStorageId(storageId);
            artifactSliceDownloadInfoDto.setRepositoryId(repositoryId);
            artifactSliceDownloadInfoDto.setPath(path);
            artifactSliceDownloadInfoDto.setUsedSlice(artifactFileLength > kbps);
            artifactSliceDownloadInfoDto.setArtifactMd5(md5);

            if (artifactSliceDownloadInfoDto.getUsedSlice()) {
                try {
                    final String artifactParentUri = Optional.of(artifactPath.relativize()).map(p -> {
                        try {
                            return p.getParent().toString();
                        } catch (Exception e) {
                            return StringUtils.EMPTY;
                        }
                    }).get();
                    final String sliceStoreFolderUri = String.format("%s.slice", StringUtils.isNotBlank(artifactParentUri) ? artifactParentUri + "/" : StringUtils.EMPTY);
                    final String sliceGenJsonFileUri = String.format("%s/slice-gen.json", sliceStoreFolderUri);

                    // 根据文件MD5检查是否已经生成切片数据，如有则返回生成已经存在的切片数据（避免重复生成）
                    final RepositoryPath sliceGenJsonUriPath = repositoryPathResolver.resolve(storageId, repositoryId, sliceGenJsonFileUri);
                    if (Files.exists(sliceGenJsonUriPath)) {
                        try (InputStream inputStream = Files.newInputStream(sliceGenJsonUriPath)) {
                            final String sliceGenJson = IoUtil.readUtf8(inputStream);
                            if (StringUtils.isNotBlank(sliceGenJson)) {
                                final ArtifactSliceDownloadInfoRes cacheDto = JSON.parseObject(sliceGenJson, ArtifactSliceDownloadInfoRes.class);
                                if (null != cacheDto && StringUtils.isNotBlank(md5) && md5.equals(cacheDto.getArtifactMd5())) {
                                    if (CollUtil.isNotEmpty(cacheDto.getDownloadPartList())) {
                                        for (ArtifactSliceDownloadInfoRes.DownloadPartInfo downloadPartInfo : cacheDto.getDownloadPartList()) {
                                            downloadPartInfo.setDownloadUrl(String.format("%s/storages/%s/%s/%s", baseUrl, storageId, repositoryId, downloadPartInfo.getDownloadUri()));
                                        }
                                    }
                                    return cacheDto;
                                }
                            }
                        }
                    }

                    if (S3FileSystemStorageProvider.ALIAS.equals(repository.getStorageProvider())) {
                        // 由于是网络路径，需要暂存到本地进行暂存
                        artifactFilePath = String.format("%s/artifactTemp/%s/%s", StringUtils.chomp(tempPath, "/"), UUID.randomUUID().toString(true), fileName);
                        try (InputStream inputStream = new BufferedInputStream(Files.newInputStream(artifactPath))) {
                            FileUtil.writeFromStream(inputStream, artifactFilePath);
                        }
                    }
                    final List<String> splitFilePathList = FileUtils.splitFile(artifactFilePath, artifactFileSliceFolderPath, kbps);

                    // 将暂存的文件
                    log.info("splitFilePathList>>> {}", JSON.toJSONString(splitFilePathList));
                    final boolean result = splitFilePathList.stream().allMatch(splitFilePath -> {
                        final String splitFileName = FileUtil.getName(splitFilePath);
                        final String splitFileStoreUri = String.format("%s/%s", sliceStoreFolderUri, splitFileName);
                        final RepositoryPath splitFileStorePath = repositoryPathResolver.resolve(storageId, repositoryId, splitFileStoreUri);
                        try (InputStream inputStream = Files.newInputStream(Path.of(splitFilePath))) {
                            artifactManagementService.store(splitFileStorePath, inputStream);
                        } catch (IOException e) {
                            log.error("转存切片文件（{} => {}）失败", splitFilePath, splitFileStoreUri, e);
                            return false;
                        }
                        return true;
                    });
                    if (!result) {
                        throw new BusinessException("转存切片文件失败");
                    }

                    // 生成下载路径
                    final List<ArtifactSliceDownloadInfoRes.DownloadPartInfo> downloadPartInfoList = splitFilePathList.stream()
                            .map(splitFilePath -> {
                                final String splitFileName = FileUtil.getName(splitFilePath);
                                final String splitFileStoreUri = String.format("%s/%s", sliceStoreFolderUri, splitFileName);
                                return new ArtifactSliceDownloadInfoRes.DownloadPartInfo()
                                        .setDownloadUri(splitFileStoreUri)
                                        .setDownloadUrl(String.format("%s/storages/%s/%s/%s", baseUrl, storageId, repositoryId, splitFileStoreUri));
                            })
                            .collect(Collectors.toList());
                    artifactSliceDownloadInfoDto.setDownloadPartList(downloadPartInfoList);

                    // 持久化切片数据
                    artifactManagementService.store(repositoryPathResolver.resolve(storageId, repositoryId, sliceGenJsonFileUri),
                            new StringInputStream(JSON.toJSONString(artifactSliceDownloadInfoDto)));

                } catch (IOException e) {
                    log.error("切片制品文件失败", e);
                    throw new BusinessException("切片制品文件失败");
                }
            } else {
                final String artifactUri = String.format("%s/%s/%s", storageId, repositoryId, artifactPath.relativize());
                artifactSliceDownloadInfoDto.setDownloadPartList(Collections.singletonList(
                        new ArtifactSliceDownloadInfoRes.DownloadPartInfo()
                                .setDownloadUri(artifactUri)
                                .setDownloadUrl(String.format("%s/storages/%s", baseUrl, artifactUri))
                ));
            }
        } catch (BusinessException e) {
            throw e;
        } catch (Exception e) {
            log.error("获取制品切片下载信息失败", e);
            throw new BusinessException("获取制品切片下载信息失败");
        }

        return artifactSliceDownloadInfoDto;
    }

    @Override
    public List<ArtifactSliceDownloadInfoRes> batchQuerySliceDownloadInfo
            (List<ArtifactSliceDownloadInfoReq> models) {
        return models.stream().map(this::querySliceDownloadInfoStoreTemp).filter(Objects::nonNull).collect(Collectors.toList());
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

}
