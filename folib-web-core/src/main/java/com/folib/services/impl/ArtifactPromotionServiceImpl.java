/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.services.impl;

import cn.hutool.core.io.FileUtil;
import cn.hutool.core.io.IORuntimeException;
import cn.hutool.core.io.IoUtil;
import cn.hutool.core.lang.UUID;
import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.hazelcast.core.HazelcastInstance;
import com.hazelcast.map.IMap;
import com.folib.components.artifact.ArtifactComponent;
import com.folib.components.security.SecurityComponent;
import com.folib.domain.AnalysisHtmlGetDirAndFilePath;
import com.folib.domain.ArtifactParse;
import com.folib.domain.ArtifactPromotion;
import com.folib.domain.PromotionFileRelativePath;
import com.folib.dto.ArtifactDto;
import com.folib.dto.TargetRepositoyDto;
import com.folib.entity.Dict;
import com.folib.enums.BusinessCodeEnum;

import com.folib.model.request.ArtifactSliceDownloadInfoReq;
import com.folib.model.request.ArtifactSliceUploadReq;
import com.folib.model.request.ArtifactSliceUploadWebReq;
import com.folib.model.response.ArtifactSliceDownloadInfoRes;
import com.folib.model.response.ArtifactSliceUploadInfoRes;
import com.folib.promotion.ArtifactUploadTask;
import com.folib.promotion.PromotionUtil;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.providers.layout.LayoutProviderRegistry;
import com.folib.repositories.ArtifactRepository;
import com.folib.repository.MavenRepositoryFeatures;
import com.folib.scanner.common.exception.BusinessException;
import com.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.folib.services.ArtifactManagementService;
import com.folib.services.ArtifactMetadataService;
import com.folib.services.ArtifactPromotionService;
import com.folib.services.ArtifactResolutionService;
import com.folib.services.ArtifactWebService;
import com.folib.services.ConfigurationManagementService;
import com.folib.services.DictService;
import com.folib.services.RepositoryManagementService;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryTypeEnum;
import com.folib.users.userdetails.SpringSecurityUser;
import com.folib.util.MessageDigestUtils;
import com.folib.util.UriUtils;
import com.folib.utils.FileUtils;
import com.folib.utils.PropertiesUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.codec.digest.MessageDigestAlgorithms;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.maven.model.Model;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;
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


import javax.inject.Inject;

import jakarta.servlet.http.HttpServletResponse;
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
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.FutureTask;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

/**
 * @author veadan
 */
@Service
@Slf4j
public class ArtifactPromotionServiceImpl implements ArtifactPromotionService {

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

    @Value("${folib.temp}")
    private String tempPath;

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
    private ConfigurationManagementService configurationManagementService;

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
    public ArtifactParse parseArtifact(String storageId, String repositoryId, MultipartFile file) {
        String uuid = UUID.fastUUID().toString();
        String fileOriginalName = file.getOriginalFilename();
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
                String fileOriginalName =  file.getOriginalFilename();
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

    private int getDownloadSpeedByte(int limitKbps, int downloadThreadCount) {
        return limitKbps / downloadThreadCount;
    }

    private void sliceSpeedLimitDownload(InputStream inputStream, OutputStream outputStream, AtomicInteger
            downloadConnectionCounter, int finalKbps) {
        try (final OutputStream outputStream1 = outputStream;
             final InputStream inputStream1 = inputStream;) {
            int speedByteSize = this.getDownloadSpeedByte(finalKbps, downloadConnectionCounter.incrementAndGet());
            final byte[] speedBytes = new byte[finalKbps];

            int offset;
            while ((offset = inputStream1.read(speedBytes, 0, speedByteSize)) != -1) {
                TimeUnit.SECONDS.sleep(1);
                // 获取下一秒下载速度
                speedByteSize = this.getDownloadSpeedByte(finalKbps, downloadConnectionCounter.get());
                outputStream1.write(speedBytes, 0, offset);
            }
        } catch (Exception e) {
            log.error("下载切片文件失败", e);
        }
    }
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




    private void validateSourceRepositoryPath(String storageId, String repositoryId, String artifactPath) {
        validateStorageAndRepository(storageId, repositoryId);
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
        if (Objects.isNull(repositoryPath) || !Files.exists(repositoryPath)) {
            throw new BusinessException(String.format("Source repositoryPath [%s] [%s] [%s]  not exist!", storageId, repositoryId, artifactPath));
        }
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
