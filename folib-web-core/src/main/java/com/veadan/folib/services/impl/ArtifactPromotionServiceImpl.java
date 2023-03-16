package com.veadan.folib.services.impl;

import com.alibaba.fastjson.JSON;
import com.veadan.folib.domain.*;
import com.veadan.folib.dto.ArtifactDto;
import com.veadan.folib.dto.PromotionArtifactDto;
import com.veadan.folib.dto.PromotionNodeOptionDto;
import com.veadan.folib.dto.TargetRepositoyDto;
import com.veadan.folib.entity.Dict;
import com.veadan.folib.promotion.ArtifactUploadTask;
import com.veadan.folib.promotion.PromotionUtil;
import com.veadan.folib.promotion.PullArtifactTask;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.layout.LayoutProviderRegistry;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.veadan.folib.services.*;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.compress.utils.Lists;
import org.apache.commons.fileupload.disk.DiskFileItem;
import org.apache.commons.lang3.StringUtils;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.multipart.commons.CommonsMultipartFile;

import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.Entity;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.FutureTask;

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

    @Value("${folib.temp}")
    private String tempPath;

    @Value("${folib.host:localhost}")
    private String host;

    @Value("${folib.port}")
    private int port;

    @Inject
    private DictService dictService;


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
            String sourcePath = promotionNodeOption.getSourcePath();
            String targetPath = promotionNodeOption.getTargetPath();
            String srcStorageId = sourcePath.split(":")[2].split("/")[1];
            String srcRepostoryId = sourcePath.split(":")[2].split("/")[2];
            String srcUrl = sourcePath.split("/" + srcStorageId + "/" + srcRepostoryId + "/")[0];
            String srcUri = sourcePath.split("/" + srcStorageId + "/" + srcRepostoryId + "/")[1];
            String targetStorageId = targetPath.split(":")[2].split("/")[1];
            String targetRepostoryId = targetPath.split(":")[2].split("/")[2];
            String targetUrl = targetPath.split("/" + targetStorageId + "/" + targetRepostoryId + "/")[0];
            String targetUri = targetPath.split("/" + targetStorageId + "/" + targetRepostoryId + "/")[1];
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
            String requestURL = request.getRequestURL().toString().replace(request.getRequestURI(), "");
            if (sourcePath.contains(requestURL)) {
                validateStorageAndRepository(srcStorageId, srcRepostoryId);
                // 本地源 制品路径 推向 目标路径
                Storage srcStorage = repositoryManagementService.getStorage(srcStorageId);// todo validate
                Repository srcRepository = repositoryManagementService.getStorage(srcStorageId).getRepository(srcRepostoryId);
                RepositoryPath srcPath = repositoryPathResolver.resolve(srcRepository, srcUri);
                //  遍历所有制品文件后逐步上传
                String srcAbsolutePath = srcPath.getTarget().toString();
                PromotionArtifactDto promotionArtifactDto = new PromotionArtifactDto(srcStorageId, srcRepostoryId,
                        targetStorageId, targetRepostoryId, srcAbsolutePath, targetUrl + upLoadURI);

                PromotionNodeOptionDto uploadDto = promotionUtil.getPromotionUploadDto(promotionArtifactDto);

                //向目标仓库推包
                promotionUtil.upload(targetUrl + upLoadURI, uploadDto);

            } else if (targetPath.contains(requestURL)) {
                validateStorageAndRepository(targetStorageId, targetRepostoryId);
                // 从源仓路径 pull 到目标仓路径 获取目标主机的path 路径下的文件与目录 然后依次提交到任务队列里面后将文件存入仓库
                String url = srcUrl + getFileRelativePaths;
                Client client = clientPool.getRestClient();
                WebTarget target = client.target(url);
                ArtifactDto artifactDto = ArtifactDto.builder().storageId(srcStorageId).
                        repostoryId(srcRepostoryId).path(srcUri).build();
                Response response = target.request().
                        post(Entity.entity(artifactDto, MediaType.APPLICATION_JSON));
                if (response.getStatus() != 200) {
                    throw new Exception("{} get error" + url);
                }
                PromotionFileRelativePath promotionFileRelativePath = response.readEntity(PromotionFileRelativePath.class);
                List<String> getFileRelativePaths = promotionFileRelativePath.getList();
                Map<String, Object> metaDataMap = promotionFileRelativePath.getMetaData();

                // 添加task
                List<FutureTask<String>> listTask = new ArrayList<>();
                for (String path : getFileRelativePaths) {
                    ArtifactDto artifac = ArtifactDto.builder().storageId(srcStorageId)
                            .repostoryId(srcRepostoryId).path(path).build();
                    String fileUlr = srcUrl + "/api/artifact/folib/promotion/download";
                    String metaData = metaDataMap.getOrDefault(path, "") == null ?
                            "" : metaDataMap.getOrDefault(path, "").toString();
                    PullArtifactTask pullArtifactTask = new PullArtifactTask(path, fileUlr, targetStorageId,
                            targetRepostoryId, repositoryPathResolver, artifactManagementService, clientPool,
                            promotionUtil, artifac, metaData);
                    FutureTask<String> futureTask = new FutureTask<String>(pullArtifactTask);
                    listTask.add(futureTask);
                    asyncRepositoryThreadPoolExecutor.submit(futureTask);
                }
                int success = 0;
                int fail = 0;
                for (FutureTask<String> task : listTask) {
                    try {
                        task.get();
                        success++;

                    } catch (Exception e) {
                        fail++;
                        log.error("pull fail {}", e.getMessage());
                    }
                }
                log.info("Handle pulled! Task size {} success {} fail {}", listTask.size(), success, fail);
                listTask.clear();
            }
        } catch (Exception e) {
            log.error("制品晋级错误 {}", e.getMessage());
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(e.getMessage());
        }
        return ResponseEntity.ok("ok");
    }

    private AnalysisHtmlGetDirAndFilePath getArtifactPath(String url) throws Exception {
        Client client = clientPool.getRestClient();
        WebTarget target = client.target(url);
        Response response = target.request().get();
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
                        repositoryManagementService, repositoryPathResolver, artifactManagementService, promotionUtil, layoutProviderRegistry, artifactMetadataService, artifactRepository, tempPath, fileRelativePath, metaData, uuid);
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
                    log.error("upload exception {}", e.getMessage());
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
            log.error("download exception {}", e.getMessage());
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
            // 添加 docker version 请求处理
            List<String> fileNameList = Lists.newArrayList();
            fileNameList.add(artifactDto.getPath());
            boolean isDockerVersionPath = promotionUtil.isDockerVersion(repositoryPath.getRepository().getLayout(), fileNameList);
            PromotionFileRelativePath promotionFileRelativePath = promotionUtil.getFileRelativePaths(repositoryPath, isDockerVersionPath);
            return ResponseEntity.ok(promotionFileRelativePath);
        } catch (Exception e) {
            log.error("Get files relative paths exception {}", e.getMessage());
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(e.getMessage());
        }
    }

    @Override
    public ResponseEntity artifactDispatch(ArtifactDispatch artifactDispatch) {
        log.info("start artifact dispatch");
        promotionUtil.executeHandleDispatch(artifactDispatch);
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
