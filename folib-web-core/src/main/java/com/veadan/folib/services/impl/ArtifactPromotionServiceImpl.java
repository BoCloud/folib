package com.veadan.folib.services.impl;

import cn.hutool.core.io.FileUtil;
import cn.hutool.core.lang.UUID;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.components.artifact.ArtifactComponent;
import com.veadan.folib.components.promotion.ArtifactPromotionProvider;
import com.veadan.folib.components.promotion.ArtifactPromotionProviderRegistry;
import com.veadan.folib.components.security.SecurityComponent;
import com.veadan.folib.domain.AnalysisHtmlGetDirAndFilePath;
import com.veadan.folib.domain.ArtifactDispatch;
import com.veadan.folib.domain.ArtifactParse;
import com.veadan.folib.domain.ArtifactPromotion;
import com.veadan.folib.domain.PromotionFileRelativePath;
import com.veadan.folib.domain.PromotionNodeOption;
import com.veadan.folib.dto.ArtifactDto;
import com.veadan.folib.dto.ArtifactPromotionInfoDto;
import com.veadan.folib.dto.PromotionArtifactDto;
import com.veadan.folib.dto.PromotionNodeOptionDto;
import com.veadan.folib.dto.TargetDispatchRepositoryDto;
import com.veadan.folib.dto.TargetRepositoyDto;
import com.veadan.folib.entity.ArtifactSyncRecord;
import com.veadan.folib.entity.Dict;
import com.veadan.folib.enums.ArtifactSyncRecordStatusEnum;
import com.veadan.folib.enums.ArtifactSyncRecordSyncModelEnum;
import com.veadan.folib.mapper.ArtifactSyncRecordMapper;
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
import com.veadan.folib.services.DictService;
import com.veadan.folib.services.RepositoryManagementService;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.users.userdetails.SpringSecurityUser;
import com.veadan.folib.utils.PropertiesUtils;
import com.veadan.folib.utils.UrlUtils;
import com.veadan.folib.ws.common.FolibWsAction;
import com.veadan.folib.ws.client.handler.command.FolibWsClientArtifactPullCommand;
import com.veadan.folib.ws.server.manage.FolibWsClientRunManage;
import lombok.extern.slf4j.Slf4j;
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
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.multipart.commons.CommonsMultipartFile;

import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.websocket.Session;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.Invocation;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.Response;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.FutureTask;
import java.util.stream.Collectors;

import static com.veadan.folib.utils.UrlUtils.parsePath;

/**
 * @author qijianping
 */
@Service
@Slf4j
public class ArtifactPromotionServiceImpl implements ArtifactPromotionService {

    private final String upLoadURI = "/api/artifact/folib/promotion/upload-files";
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
    private ThreadPoolTaskExecutor asyncRepositoryThreadPoolExecutor;

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
                log.info("Copying {} from {}:{} to {}:{}...", artifactPromotion.getPath(), srcStorageId, srcRepositoryId, destStorageId,
                        destRepositoryId);
                singleCopy(artifactPromotion, srcRepository, destStorageId, destRepositoryId);
            });
        } catch (Exception e) {
            log.error("Unable to copy artifact", e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(e.getMessage());
        }
        return ResponseEntity.ok("Artifact copying");
    }

    private void checkParam(ArtifactPromotion artifactPromotion) throws Exception {
        final String srcStorageId = artifactPromotion.getSrcStorageId();
        final String srcRepositoryId = artifactPromotion.getSrcRepositoryId();

        if (null == repositoryManagementService.getStorage(srcStorageId)) {
            throw new Exception("The source StorageId does not exist!");
        }

        Repository srcRepository = repositoryManagementService.getStorage(srcStorageId).getRepository(srcRepositoryId);
        if (null == srcRepository) {
            throw new Exception("The source RepositoryId does not exist!");
        }

        if (!srcRepository.getType().equals("hosted")) {
            throw new Exception("The source RepositoryId does not local");
        }

        final RepositoryPath srcRepositoryPath = repositoryPathResolver.resolve(srcRepository, artifactPromotion.getPath());
        if (!Files.exists(srcRepositoryPath)) {
            throw new Exception("The source path does not exist!");
        }
        List<TargetRepositoyDto> targetList = artifactPromotion.getTargetRepositoyList();

        if (CollectionUtils.isEmpty(targetList)) {
            throw new Exception("The target is empty");
        }
        StringBuilder stringBuilder = new StringBuilder();
        for (TargetRepositoyDto dto : targetList) {
            String targetStorageId = dto.getTargetStorageId();
            String targetRepositoryId = dto.getTargetRepositoryId();
            if (null == repositoryManagementService.getStorage(targetStorageId)) {
                stringBuilder.append("Storage : ").append(targetStorageId).append(" not exits");
                continue;
            }
            Repository targetRepository = repositoryManagementService.getStorage(targetStorageId).getRepository(targetRepositoryId);
            if (null == targetRepository) {
                stringBuilder.append(System.lineSeparator()).append(" Repository : ").append(targetRepositoryId).append(" not exits");
                continue;
            }
            if (!targetRepository.getType().equals("hosted")) {
                stringBuilder.append(System.lineSeparator()).append("Repository : ").append(targetRepositoryId).append("does not local");
            }
        }
        if (StringUtils.isNotBlank(stringBuilder.toString())) {
            throw new Exception(stringBuilder.toString());
        }
    }

    private void singleCopy(ArtifactPromotion artifactPromotion, Repository srcRepository, String destStorageId, String destRepositoryId) {
        Repository destRepository = repositoryManagementService.getStorage(destStorageId).getRepository(destRepositoryId);
        RepositoryPath srcPath = repositoryPathResolver.resolve(srcRepository, artifactPromotion.getPath());
        promotionUtil.executeHanleCopy(srcPath.getTarget().toString(), destRepository, srcRepository);
    }

    @Override
    public ResponseEntity move(ArtifactPromotion artifactPromotion) {
        try {
            checkParam(artifactPromotion);
            promotionUtil.executeHandleMove(artifactPromotion);
        } catch (Exception e) {
            log.error("Unable to move artifact", e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(e.getMessage());
        }
        return ResponseEntity.ok("Artifact moving");
    }

    @Override
    public ResponseEntity nodeOption(PromotionNodeOption promotionNodeOption, HttpServletRequest request) {
        try {
            String sourcePath = StringUtils.removeEnd(promotionNodeOption.getSourcePath(), "/");
            String targetPath = StringUtils.removeEnd( promotionNodeOption.getTargetPath(), "/");
            final Integer syncModel = promotionNodeOption.getSyncModel();
            String srcStorageId = parsePath(sourcePath)[0];
            String srcRepostoryId = parsePath(sourcePath)[1];
            String srcUrl = sourcePath.split("/" + srcStorageId + "/" + srcRepostoryId + "/")[0];
            String srcUri = sourcePath.split("/" + srcStorageId + "/" + srcRepostoryId + "/")[1];
            String targetStorageId =  parsePath(targetPath)[0];
            String targetRepostoryId = parsePath(targetPath)[1];
            String targetUrl = targetPath.split("/" + targetStorageId + "/" + targetRepostoryId + "/")[0];
            String targetUri = targetPath.split("/" + targetStorageId + "/" + targetRepostoryId + "/")[1];

            log.info("sourcePath={},srcStorageId={},srcRepostoryId={}\ntargetPath={},targetStorageId={},targetRepostoryId={}",sourcePath,srcStorageId,srcRepostoryId,targetStorageId,targetStorageId,targetRepostoryId);
            log.info("srcUrl={},srcUri={}",srcUrl,srcUri);
            log.info("targetUrl={},targetUri={}",targetUrl,targetUri);
            if (srcUrl.equals(targetUrl)) {
                validateStorageAndRepository(srcStorageId, srcRepostoryId);
                validateStorageAndRepository(targetStorageId, targetRepostoryId);
                Repository destRepository = repositoryManagementService.getStorage(targetStorageId).getRepository(targetRepostoryId);
                Repository srcRepository = repositoryManagementService.getStorage(srcStorageId).getRepository(srcRepostoryId);
                RepositoryPath srcPath = repositoryPathResolver.resolve(srcStorageId, srcRepostoryId, srcUri);
                promotionUtil.executeHanleCopy(srcPath.getTarget().toString(), destRepository, srcRepository);
                return ResponseEntity.ok("ok");
            }

            // 判断节点参数是 做推 push  或者 拉取 pull
//            String requestURL = request.getServerName();
//            log.info("requestURL={}",requestURL);

//            if (sourcePath.contains(requestURL)) {
            if (ArtifactSyncRecordSyncModelEnum.PUSH.getVal().equals(syncModel)) {
                log.info("进入推模式={}",true);
                validateStorageAndRepository(srcStorageId, srcRepostoryId);
                // 本地源 制品路径 推向 目标路径
                Repository srcRepository = repositoryManagementService.getStorage(srcStorageId).getRepository(srcRepostoryId);
                RepositoryPath srcPath = repositoryPathResolver.resolve(srcRepository, srcUri);
                //  遍历所有制品文件后逐步上传
                String srcAbsolutePath = srcPath.getTarget().toString();
                PromotionArtifactDto promotionArtifactDto = new PromotionArtifactDto(srcStorageId, srcRepostoryId,
                        targetStorageId, targetRepostoryId, srcAbsolutePath, targetUrl + upLoadURI);

                PromotionNodeOptionDto uploadDto = promotionUtil.getPromotionUploadDto(promotionArtifactDto);

                //向目标仓库推包
                promotionUtil.upload(targetUrl + upLoadURI, uploadDto);

//            } else if (targetPath.contains(requestURL)) {
            } else if (ArtifactSyncRecordSyncModelEnum.PULL.getVal().equals(syncModel)) {
                log.info("进入拉模式={}",true);
                // 通过Ws协议通知客户端进行拉取操作
                final URL url = new URL(targetUrl);
                final String targetHost = url.getHost();
                final Integer targetPort = UrlUtils.getPort(targetUrl);
                final String nodeName = String.format("%s:%s", targetHost, targetPort);
                final FolibWsClientRunManage.FolibWsClientRun wsClientRun = FolibWsClientRunManage.getWsClientRun(nodeName);
                if (null == wsClientRun)
                { throw new BusinessException("需要晋级的节点不可用，请检查节点是否配置正确"); }
                final Session session = wsClientRun.getSession();
                session.getBasicRemote().sendText(new FolibWsAction()
                        .command(FolibWsClientArtifactPullCommand.COMMAND)
                        .payload(promotionNodeOption)
                        .encode());
                
///                validateStorageAndRepository(targetStorageId, targetRepostoryId);
///                // 从源仓路径 pull 到目标仓路径 获取目标主机的path 路径下的文件与目录 然后依次提交到任务队列里面后将文件存入仓库
///                String url = srcUrl + getFileRelativePaths;
///                Client client = clientPool.getRestClient();
///                WebTarget target = client.target(url);
///                ArtifactDto artifactDto = ArtifactDto.builder().storageId(srcStorageId).
///                        repostoryId(srcRepostoryId).path(srcUri).build();
///                Invocation.Builder builder = target.request();
///                securityComponent.securityTokenHeader(builder);
///                Response response = builder.
///                        post(Entity.entity(artifactDto, MediaType.APPLICATION_JSON));
///                if (response.getStatus() != 200) {
///                    throw new Exception("{} get error" + url);
///                }
///                PromotionFileRelativePath promotionFileRelativePath = response.readEntity(PromotionFileRelativePath.class);
///                List<String> getFileRelativePaths = promotionFileRelativePath.getList();
///                Map<String, Object> metaDataMap = promotionFileRelativePath.getMetaData();
///
///                // 添加task
///                List<FutureTask<String>> listTask = new ArrayList<>();
///                for (String path : getFileRelativePaths) {
///                    ArtifactDto artifac = ArtifactDto.builder().storageId(srcStorageId)
///                            .repostoryId(srcRepostoryId).path(path).build();
///                    String fileUlr = srcUrl + "/api/artifact/folib/promotion/download";
///                    String metaData = metaDataMap.getOrDefault(path, "") == null ?
///                            "" : metaDataMap.getOrDefault(path, "").toString();
///                    PullArtifactTask pullArtifactTask = new PullArtifactTask(path, fileUlr, targetStorageId,
///                            targetRepostoryId, repositoryPathResolver, artifactManagementService, clientPool,
///                            promotionUtil, artifac, metaData);
///                    FutureTask<String> futureTask = new FutureTask<String>(pullArtifactTask);
///                    listTask.add(futureTask);
///                    asyncRepositoryThreadPoolExecutor.submit(futureTask);
///                }
///                int success = 0;
///                int fail = 0;
///                for (FutureTask<String> task : listTask) {
///                    try {
///                        task.get();
///                        success++;
///
///                    } catch (Exception e) {
///                        fail++;
///                        log.error("pull fail {}", e.getMessage());
///                    }
///                }
///                log.info("Handle pulled! Task size {} success {} fail {}", listTask.size(), success, fail);
///                listTask.clear();
            }
        } catch (Exception e) {
            log.error("制品晋级错误 {}", ExceptionUtils.getStackTrace(e));
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(e.getMessage());
        }
        return ResponseEntity.ok("ok");
    }

    @Override
    public ResponseEntity nodeOptionAttachRecord(PromotionNodeOption promotionNodeOption, HttpServletRequest request) 
    {
        // 生成同步编号
        final String syncNo = String.format("SyncNo-%s", UUID.fastUUID());
        final SpringSecurityUser userDetails = (SpringSecurityUser) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        final String userName = Optional.ofNullable(userDetails).map(SpringSecurityUser::getUsername).orElse(null);

        // 生成日志记录
        final ArtifactSyncRecord artifactSyncRecord = new ArtifactSyncRecord();
        artifactSyncRecord.setSourcePath(promotionNodeOption.getSourcePath());
        artifactSyncRecord.setTargetPath(promotionNodeOption.getTargetPath());
        artifactSyncRecord.setSyncNo(syncNo);
        artifactSyncRecord.setSyncModel(promotionNodeOption.getSyncModel());
        artifactSyncRecord.setStatus(ArtifactSyncRecordStatusEnum.IN_SYNC.getVal());
        artifactSyncRecord.setCreatedBy(userName);
        artifactSyncRecord.setCreatedTime(new Date());
        artifactSyncRecordMapper.insert(artifactSyncRecord);

        try 
        {
            asyncRepositoryThreadPoolExecutor.execute(() -> 
            { // 异步执行制品晋级
                ResponseEntity re = this.nodeOption(promotionNodeOption, request);
                if (HttpStatus.OK.equals(re.getStatusCode())) 
                { artifactSyncRecord.setStatus(ArtifactSyncRecordStatusEnum.SUCCESS.getVal()); }
                else
                { 
                    artifactSyncRecord.setStatus(ArtifactSyncRecordStatusEnum.FAILED.getVal()); 
                    if (Objects.nonNull(re.getBody()))
                    { artifactSyncRecord.setFailedReason(re.getBody().toString()); } 
                }

                // 更新日志结束开始时间
                artifactSyncRecordMapper.updateByPrimaryKey(artifactSyncRecord
                        .setUpdatedTime(new Date())
                        .setUpdatedBy(userName));
            });
        }catch (Exception e)
        {
            artifactSyncRecord.setStatus(ArtifactSyncRecordStatusEnum.FAILED.getVal());
            artifactSyncRecord.setFailedReason(e.getMessage());

            // 更新日志结束开始时间
            artifactSyncRecordMapper.updateByPrimaryKey(artifactSyncRecord
                    .setUpdatedTime(new Date())
                    .setUpdatedBy(userName));
        }

        return ResponseEntity.ok(syncNo);
    }

    @Override
    public ResponseEntity artifactPromotionInfo(String syncNo) 
    {
        final ArtifactSyncRecord artifactSyncRecord = artifactSyncRecordMapper.selectOne(new ArtifactSyncRecord().setSyncNo(syncNo));
        if (null == artifactSyncRecord)
        {
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
    public ResponseEntity upload(MultipartFile[] files, String storageId, String repositoryId, String filePathMap, String fileMetaDataMap, String uuid) {
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
                asyncRepositoryThreadPoolExecutor.submit(task);
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
            asyncRepositoryThreadPoolExecutor.submit(futureTask);
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
            boolean isDockerVersionPath = promotionUtil.isDockerVersion(repositoryPath.getRepository().getLayout(), artifactDto.getPath());
            PromotionFileRelativePath promotionFileRelativePath = promotionUtil.getFileRelativePaths(repositoryPath, isDockerVersionPath);
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
        Map<String, List<TargetDispatchRepositoryDto>> groupByMap = artifactDispatch.getTargetDispatchRepositoryList().stream().collect(Collectors.groupingBy(TargetDispatchRepositoryDto::getArtifactoryRepositoryType));
        for(Map.Entry<String, List<TargetDispatchRepositoryDto>> item : groupByMap.entrySet()) {
            ArtifactPromotionProvider artifactPromotionProvider = artifactPromotionProviderRegistry.getProvider(item.getKey());
            ArtifactDispatch itemArtifactDispatch = new ArtifactDispatch();
            BeanUtils.copyProperties(artifactDispatch, itemArtifactDispatch);
            itemArtifactDispatch.setTargetDispatchRepositoryList(item.getValue());
            artifactPromotionProvider.dispatch(itemArtifactDispatch);
        }
        return ResponseEntity.ok("ok");
    }

    @Override
    public void validateStorageAndRepository(String storageId, String repositoryId) throws Exception {
        if (null == repositoryManagementService.getStorage(storageId)) {
            throw new Exception("Storage [" + storageId + "] not exist!");
        }
        Repository repository = repositoryManagementService.getStorage(storageId).getRepository(repositoryId);
        if (null == repository) {
            throw new Exception("Repository [" + repositoryId + "]  not exist!");
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
}
