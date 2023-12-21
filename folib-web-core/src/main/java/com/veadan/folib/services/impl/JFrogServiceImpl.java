package com.veadan.folib.services.impl;

import cn.hutool.core.collection.CollectionUtil;
import cn.hutool.core.io.FileUtil;
import cn.hutool.core.util.URLUtil;
import cn.hutool.json.JSONUtil;
import com.alibaba.fastjson.JSONObject;
import com.google.common.collect.Maps;
import com.veadan.folib.artifact.coordinates.DockerArtifactCoordinates;
import com.veadan.folib.components.artifact.ArtifactComponent;
import com.veadan.folib.components.layout.DockerComponent;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.enums.PromotionStatusEnum;
import com.veadan.folib.forms.externalnode.ExternalNodeForm;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.layout.DockerLayoutProvider;
import com.veadan.folib.scanner.common.exception.BusinessException;
import com.veadan.folib.schema2.ImageManifest;
import com.veadan.folib.schema2.LayerManifest;
import com.veadan.folib.services.ExternalNodeService;
import com.veadan.folib.services.JFrogService;
import com.veadan.folib.util.RSAUtils;
import com.veadan.folib.utils.DockerApiHeader;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.codec.digest.MessageDigestAlgorithms;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.http.Header;
import org.apache.http.client.HttpResponseException;
import org.jfrog.artifactory.client.Artifactory;
import org.jfrog.artifactory.client.ArtifactoryClientBuilder;
import org.jfrog.artifactory.client.ArtifactoryRequest;
import org.jfrog.artifactory.client.ArtifactoryResponse;
import org.jfrog.artifactory.client.impl.ArtifactoryRequestImpl;
import org.jfrog.artifactory.client.impl.CopyMoveException;
import org.jfrog.artifactory.client.model.CopyMoveResultMessage;
import org.jfrog.artifactory.client.model.CopyMoveResultReport;
import org.jfrog.artifactory.client.model.File;
import org.jfrog.artifactory.client.model.LightweightRepository;
import org.jfrog.artifactory.client.model.impl.RepositoryTypeImpl;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;

import javax.inject.Inject;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @author leipenghui
 **/
@Slf4j
@Service
public class JFrogServiceImpl implements JFrogService {

    @Inject
    private ExternalNodeService externalNodeService;

    @Inject
    private ArtifactComponent artifactComponent;

    @Inject
    private DockerComponent dockerComponent;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private RSAUtils rsaUtils;

    @Override
    public Artifactory getArtifactory(String address, String username, String password) {
        return ArtifactoryClientBuilder.create()
                .setUrl(address)
                .setUsername(username)
                .setPassword(password)
                .setConnectionTimeout(10000)
                .build();
    }

    @Override
    public void validateArtifactory(String address, String username, String password) {
        listRepository(address, username, password);
    }

    @Override
    public List<LightweightRepository> listRepository(String address, String username, String password) {
        Artifactory artifactory = getArtifactory(address, username, password);
        return getLocalRepositoryList(artifactory);
    }

    @Override
    public List<LightweightRepository> listRepository(String address, String username, String password, List<String> packageTypes) {
        Artifactory artifactory = getArtifactory(address, username, password);
        return getLocalRepositoryList(artifactory, packageTypes);
    }

    @Override
    public boolean existsRepository(String repositoryName, Artifactory artifactory) {
        List<LightweightRepository> localRepositoryList = getLocalRepositoryList(artifactory);
        return Optional.ofNullable(localRepositoryList).orElse(Collections.emptyList()).stream().anyMatch(repo -> repo.getKey().equals(repositoryName));
    }

    @Override
    public boolean uploadItem(String nodeName, String repositoryName, RepositoryPath repositoryPath, String artifactPath, Boolean recordStatus) {
        ExternalNodeForm externalNodeForm = getExternalNodeForm(nodeName);
        if (Objects.isNull(externalNodeForm)) {
            throw new BusinessException(String.format("制品库[%s]节点信息不存在", nodeName));
        }
        String address = externalNodeForm.getAddress(), username = externalNodeForm.getUsername(), password = externalNodeForm.getPassword();
        Artifactory artifactory = getArtifactory(address, username, password);
        if (DockerLayoutProvider.ALIAS.equalsIgnoreCase(repositoryPath.getRepository().getLayout())) {
            //Docker镜像
            boolean flag = true;
            try {
                uploadImageTag(artifactory, repositoryName, repositoryPath);
                if (Boolean.TRUE.equals(recordStatus)) {
                    artifactComponent.handlerArtifactPromotion(nodeName, repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), artifactPath, PromotionStatusEnum.SUCCESS.getStatus());
                }
            } catch (Exception ex) {
                flag = false;
                log.error(ExceptionUtils.getStackTrace(ex));
                if (Boolean.TRUE.equals(recordStatus)) {
                    artifactComponent.handlerArtifactPromotion(nodeName, repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), artifactPath, PromotionStatusEnum.FAIL.getStatus());
                }
            }
            return flag;
        }
        artifactPath = URLUtil.encode(artifactPath);
        //TODO 使用checksum校验，已存在跳过上传制品，仅上传元数据
        try (InputStream inputStream = FileUtil.getInputStream(repositoryPath)) {
            File file = artifactory.repository(repositoryName).upload(artifactPath, inputStream).bySha1Checksum(repositoryPath.getArtifactEntry().getChecksums().get(MessageDigestAlgorithms.SHA_1)).doUpload();
            if (Objects.nonNull(file)) {
                syncMetadata(artifactory, repositoryName, repositoryPath);
            }
            if (Boolean.TRUE.equals(recordStatus)) {
                if (Objects.nonNull(file)) {
                    artifactComponent.handlerArtifactPromotion(nodeName, repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), artifactPath, PromotionStatusEnum.SUCCESS.getStatus());
                } else {
                    artifactComponent.handlerArtifactPromotion(nodeName, repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), artifactPath, PromotionStatusEnum.FAIL.getStatus());
                }
            }
            return Objects.nonNull(file);
        } catch (Exception ex) {
            log.error("上传制品失败：{}", ExceptionUtils.getStackTrace(ex));
            log.info("存储空间：{} 仓库：{} 制品：{} 目标节点：{} 目标节点类型：{} 目标仓库：{} 目标路径：{} 上传结果：{}", repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), artifactPath, externalNodeForm.getNodeName(), externalNodeForm.getType(), repositoryName, artifactPath, PromotionStatusEnum.FAIL.getStatus());
            getExceptionCode(ex);
            throw new BusinessException("制品上传失败:" + ex.getMessage());
        }
    }

    /**
     * 查询仓库列表
     *
     * @param artifactory artifactory
     * @return 仓库列表
     */
    private List<LightweightRepository> getLocalRepositoryList(Artifactory artifactory) {
        List<LightweightRepository> localRepositoryList;
        try {
            localRepositoryList = artifactory.repositories().list(RepositoryTypeImpl.LOCAL);
        } catch (Exception ex) {
            log.error("查询仓库列表错误：{}", ExceptionUtils.getStackTrace(ex));
            getExceptionCode(ex);
            throw new BusinessException("制品库连接失败，请检查地址、账号、密码");
        }
        return localRepositoryList;
    }

    /**
     * 查询仓库列表
     *
     * @param artifactory  artifactory
     * @param packageTypes 仓库类型
     * @return 仓库列表
     */
    private List<LightweightRepository> getLocalRepositoryList(Artifactory artifactory, List<String> packageTypes) {
        List<LightweightRepository> localRepositoryList = getLocalRepositoryList(artifactory);
        return Optional.ofNullable(localRepositoryList).orElse(Collections.emptyList()).stream().filter(item -> packageTypes.stream().anyMatch(type -> type.equalsIgnoreCase(item.getPackageType()))).collect(Collectors.toList());
    }

    /**
     * 获取错误信息
     *
     * @param e 异常
     */
    public void getExceptionCode(Exception e) {
        if (e instanceof HttpResponseException) {
            int code = ((HttpResponseException) e).getStatusCode();
            if (code == HttpStatus.UNAUTHORIZED.value()) {
                throw new BusinessException("操作失败：身份信息验证失败");
            }
            if (code == HttpStatus.FORBIDDEN.value()) {
                throw new BusinessException("操作失败：权限不足，操作被禁止");
            }
            if (code == HttpStatus.NOT_FOUND.value()) {
                throw new BusinessException("操作失败：jfrog仓库不存在或已被删除");
            }
        }
        if (e instanceof CopyMoveException) {
            CopyMoveResultReport copyMoveResultReport = ((CopyMoveException) e).getCopyMoveResultReport();
            List<CopyMoveResultMessage> messages = copyMoveResultReport.getMessages();
            if (CollectionUtil.isEmpty(messages)) {
                return;
            }
            StringBuilder message = new StringBuilder();
            messages.forEach(msg -> message.append(msg.getMessage()));
            String permissions = "permissions";
            if (message.indexOf(permissions) >= 0) {
                throw new BusinessException("操作失败:权限不足，操作被禁止");
            }
        }
    }

    /**
     * 查询制品库节点信息
     *
     * @param nodeName 节点名称
     * @return 制品库节点信息
     */
    private ExternalNodeForm getExternalNodeForm(String nodeName) {
        ExternalNodeForm externalNode = externalNodeService.getExternalNode(ExternalNodeForm.builder().nodeName(nodeName).build());
        if (Objects.nonNull(externalNode)) {
            externalNode.setPassword(rsaUtils.decrypt(externalNode.getPassword()));
        }
        return externalNode;
    }

    private void uploadImageTag(Artifactory artifactory, String repositoryName, RepositoryPath repositoryPath) throws Exception {
        boolean isDockerTag = DockerArtifactCoordinates.isDockerTag(repositoryPath);
        if (!isDockerTag) {
            return;
        }
        if (Files.isDirectory(repositoryPath)) {
            repositoryPath = dockerComponent.getManifestPath(repositoryPath);
            if (Objects.isNull(repositoryPath) || !Files.exists(repositoryPath)) {
                return;
            }
        }
        List<ImageManifest> imageManifestList = dockerComponent.getImageManifests(repositoryPath);
        if (CollectionUtils.isEmpty(imageManifestList)) {
            return;
        }
        DockerArtifactCoordinates dockerArtifactCoordinates = DockerArtifactCoordinates.parse(RepositoryFiles.relativizePath(repositoryPath));
        if (Objects.isNull(dockerArtifactCoordinates)) {
            return;
        }
        String imageName = dockerArtifactCoordinates.getName();
        for (ImageManifest imageManifest : imageManifestList) {
            if (Objects.nonNull(imageManifest.getConfig())) {
                //config layer
                doUploadLayer(artifactory, repositoryName, imageName, repositoryPathResolver.resolve(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), DockerLayoutProvider.BLOBS + "/" + imageManifest.getConfig().getDigest()));
            }
            if (CollectionUtils.isNotEmpty(imageManifest.getLayers())) {
                for (LayerManifest layerManifest : imageManifest.getLayers()) {
                    //blob layer
                    doUploadLayer(artifactory, repositoryName, imageName, repositoryPathResolver.resolve(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), DockerLayoutProvider.BLOBS + "/" + layerManifest.getDigest()));
                }
            }
            if (imageManifestList.size() > 1) {
                //manifest layer
                doUploadLayer(artifactory, repositoryName, imageName, repositoryPathResolver.resolve(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), DockerLayoutProvider.BLOBS + "/" + imageManifest.getDigest()));
            }
        }
        //manifest layer
        uploadManifest(artifactory, repositoryName, imageName, dockerArtifactCoordinates, repositoryPathResolver.resolve(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), DockerLayoutProvider.MANIFEST + "/" + repositoryPath.getFileName().toString()));
    }

    private void doUploadLayer(Artifactory artifactory, String repositoryName, String imageName, RepositoryPath dockerLayerPath) throws Exception {
        if (!Files.exists(dockerLayerPath)) {
            return;
        }
        DockerArtifactCoordinates dockerArtifactCoordinates = DockerArtifactCoordinates.parse(RepositoryFiles.relativizePath(dockerLayerPath));
        if (Objects.isNull(dockerArtifactCoordinates)) {
            return;
        }
        String digest = dockerArtifactCoordinates.getLayers(), layerUrl = "v2/%s/%s/blobs/%s", startUploadUrl = "v2/%s/%s/blobs/uploads";
        layerUrl = String.format(layerUrl, repositoryName, imageName, digest);
        log.info("LayerUrl [{}]", layerUrl);
        if (!checkLayerExist(artifactory, layerUrl, dockerLayerPath)) {
            log.info("Layer [{}] doesn't exist. Initiating upload...", dockerLayerPath.toString());
            startUploadUrl = String.format(startUploadUrl, repositoryName, imageName);
            log.info("StartUploadUrl [{}]", startUploadUrl);
            String uploadLocation = startUpload(artifactory, startUploadUrl, dockerLayerPath);
            if (uploadLocation != null) {
                log.info("Starting layer [{}] upload...", dockerLayerPath.toString());
                uploadLayer(artifactory, uploadLocation, dockerLayerPath);
            } else {
                log.warn("Layer [{}] failed to initiate upload.", dockerLayerPath.toString());
                throw new BusinessException(String.format("Layer [%s] failed to initiate upload.", dockerLayerPath.toString()));
            }
        } else {
            log.info("Layer [{}] already exists. No need to upload.", dockerLayerPath.toString());
        }
    }

    private boolean checkLayerExist(Artifactory artifactory, String checkUrl, RepositoryPath repositoryPath) {
        ArtifactoryRequest request = new ArtifactoryRequestImpl()
                .method(ArtifactoryRequest.Method.GET)
                .addHeader("X-Requested-With", "HEAD")
                .apiUrl(checkUrl);
        try {
            ArtifactoryResponse response = artifactory.restCall(request);
            log.info("Layer [{}] check layer exist response [{}]", repositoryPath.toString(), JSONObject.toJSONString(response.getStatusLine()));
            return response.getStatusLine().getStatusCode() == HttpStatus.OK.value();
        } catch (IOException e) {
            log.error(ExceptionUtils.getStackTrace(e));
            return false;
        }
    }

    private String startUpload(Artifactory artifactory, String startUploadUrl, RepositoryPath repositoryPath) {
        ArtifactoryRequest request = new ArtifactoryRequestImpl()
                .method(ArtifactoryRequest.Method.POST)
                .apiUrl(startUploadUrl);
        try {
            ArtifactoryResponse response = artifactory.restCall(request);
            log.info("Layer [{}] start upload response [{}]", repositoryPath.toString(), JSONObject.toJSONString(response.getStatusLine()));
            if (response.getStatusLine().getStatusCode() == HttpStatus.ACCEPTED.value()) {
                String locationKey = "Location", locationValue = "";
                for (Header header : response.getAllHeaders()) {
                    if (locationKey.equalsIgnoreCase(header.getName())) {
                        locationValue = header.getValue();
                        log.info("LocationValue [{}]", locationValue);
                        locationValue = locationValue.substring(locationValue.indexOf("v2/"));
                        return locationValue;
                    }
                }
                throw new BusinessException("layer location not found");
            }
        } catch (IOException e) {
            log.error(ExceptionUtils.getStackTrace(e));
        }
        return null;
    }

    private void uploadLayer(Artifactory artifactory, String uploadLocation, RepositoryPath repositoryPath) {
        try (InputStream inputStream = Files.newInputStream(repositoryPath)) {
            ArtifactoryRequest request = new ArtifactoryRequestImpl()
                    .method(ArtifactoryRequest.Method.PATCH)
                    .apiUrl(uploadLocation).requestBody(inputStream);
            ArtifactoryResponse response = artifactory.restCall(request);
            log.info("Layer [{}] upload response [{}]", repositoryPath.toString(), JSONObject.toJSONString(response.getStatusLine()));
            if (response.getStatusLine().getStatusCode() == HttpStatus.ACCEPTED.value()) {
                log.info("Layer [{}] upload successful!", repositoryPath.toString());
                completeUpload(artifactory, uploadLocation, repositoryPath);
            } else {
                log.warn("Layer [{}] upload failed. Response code [{}]", repositoryPath.toString(), response.getStatusLine().getStatusCode());
                throw new BusinessException(String.format("Layer [%s] upload failed.", repositoryPath.toString()));
            }
        } catch (IOException e) {
            log.error(ExceptionUtils.getStackTrace(e));
            throw new BusinessException(String.format("Layer [%s] upload failed.", repositoryPath.toString()));
        }
    }

    private void completeUpload(Artifactory artifactory, String uploadLocation, RepositoryPath repositoryPath) {
        uploadLocation = uploadLocation + "?digest=" + FilenameUtils.getName(repositoryPath.toString());
        log.info("UploadLocationUrl [{}]", uploadLocation);
        ArtifactoryRequest request = new ArtifactoryRequestImpl()
                .method(ArtifactoryRequest.Method.PUT)
                .apiUrl(uploadLocation);
        try {
            ArtifactoryResponse response = artifactory.restCall(request);
            log.info("Layer [{}] upload completed response [{}]", repositoryPath.toString(), JSONObject.toJSONString(response.getStatusLine()));
            if (response.getStatusLine().getStatusCode() == HttpStatus.CREATED.value()) {
                log.info("Layer [{}] upload completed!", repositoryPath.toString());
            } else {
                log.warn("Layer [{}] upload completion failed. Response code [{}]", repositoryPath.toString(), response.getStatusLine().getStatusCode());
                throw new BusinessException(String.format("Layer [%s] upload completion failed.", repositoryPath.toString()));
            }
        } catch (IOException e) {
            log.error(ExceptionUtils.getStackTrace(e));
            throw new BusinessException(String.format("Layer [%s] upload completion failed.", repositoryPath.toString()));
        }
    }

    private void uploadManifest(Artifactory artifactory, String repositoryName, String imageName, DockerArtifactCoordinates dockerArtifactCoordinates, RepositoryPath repositoryPath) {
        String uploadUrl = "v2/%s/%s/manifests/%s";
        uploadUrl = String.format(uploadUrl, repositoryName, imageName, dockerArtifactCoordinates.getTAG());
        log.info("Manifest uploadUrl [{}]", uploadUrl);
        try (InputStream inputStream = Files.newInputStream(repositoryPath)) {
            ArtifactoryRequest request = new ArtifactoryRequestImpl()
                    .method(ArtifactoryRequest.Method.PUT)
                    .apiUrl(uploadUrl)
                    .addHeader(DockerApiHeader.DOCKER_CONTENT_TYPE.key(), DockerApiHeader.DOCKER_CONTENT_TYPE.value())
                    .requestBody(inputStream);
            ArtifactoryResponse response = artifactory.restCall(request);
            log.info("Manifest [{}] upload response [{}]", repositoryPath.toString(), JSONObject.toJSONString(response.getStatusLine()));
            if (response.getStatusLine().getStatusCode() == HttpStatus.CREATED.value()) {
                log.info("Manifest [{}] upload successful!", repositoryPath.toString());
            } else {
                log.warn("Manifest [{}] upload failed. Response code [{}]", repositoryPath.toString(), response.getStatusLine().getStatusCode());
                throw new BusinessException(String.format("Manifest [%s] upload failed.", repositoryPath.toString()));
            }
        } catch (IOException e) {
            log.error(ExceptionUtils.getStackTrace(e));
            throw new BusinessException(String.format("Manifest [%s] upload failed.", repositoryPath.toString()));
        }
    }

    private void syncMetadata(Artifactory artifactory, String repositoryName, RepositoryPath repositoryPath) {
        if (Objects.isNull(repositoryPath) || !Files.exists(repositoryPath)) {
            throw new BusinessException(String.format("RepositoryPath [%s] not exists", repositoryPath.toString()));
        }
        try {
            String artifactPath = RepositoryFiles.relativizePath(repositoryPath);
            Artifact artifact = repositoryPath.getArtifactEntry();
            if (Objects.isNull(artifact)) {
                log.warn("RepositoryPath artifact [{}] not exists", repositoryPath.toString());
                return;
            }
            if (StringUtils.isBlank(artifact.getMetadata()) || !JSONUtil.isJson(artifact.getMetadata())) {
                return;
            }
            if (!artifactPath.startsWith(java.io.File.separator)) {
                artifactPath = java.io.File.separator + artifactPath;
            }
            handlerMetadata(artifactory, repositoryName, repositoryPath);
            Map<String, String> queryParams = Maps.newHashMap();
            queryParams.put("recursive", "0");
            JSONObject metadataJson = JSONObject.parseObject(artifact.getMetadata());
            if (metadataJson.keySet().size() == 0) {
                return;
            }
            StringBuilder metadataStringBuilder = new StringBuilder();
            JSONObject valueJson = null;
            String valueKey = "value";
            for (String key : metadataJson.keySet()) {
                valueJson = metadataJson.getJSONObject(key);
                if (valueJson.containsKey(valueKey) && StringUtils.isNotBlank(valueJson.getString(valueKey))) {
                    metadataStringBuilder.append(key);
                    metadataStringBuilder.append("=");
                    metadataStringBuilder.append(valueJson.getString(valueKey));
                    metadataStringBuilder.append(";");
                }
            }
            metadataStringBuilder.deleteCharAt(metadataStringBuilder.lastIndexOf(";"));
            queryParams.put("properties", metadataStringBuilder.toString());
            String metadataUrl = "/api/storage/" + repositoryName + artifactPath;
            log.info("Sync metadata url [{}] params [{}]", metadataUrl, queryParams.toString());
            ArtifactoryRequest request = new ArtifactoryRequestImpl()
                    .method(ArtifactoryRequest.Method.PUT)
                    .apiUrl(metadataUrl)
                    .setQueryParams(queryParams);
            ArtifactoryResponse response = artifactory.restCall(request);
            log.info("Sync metadata [{}] response [{}]", repositoryPath.toString(), JSONObject.toJSONString(response.getStatusLine()));
            if (response.getStatusLine().getStatusCode() == HttpStatus.NO_CONTENT.value()) {
                log.info("Sync metadata [{}] successful!", repositoryPath.toString());
            } else {
                log.warn("Sync metadata [{}] failed. Response code [{}]", repositoryPath.toString(), response.getStatusLine().getStatusCode());
                throw new BusinessException(String.format("Sync metadata [%s] failed.", repositoryPath.toString()));
            }
        } catch (IOException e) {
            log.error(ExceptionUtils.getStackTrace(e));
            throw new BusinessException(String.format("Sync metadata [%s] failed.", repositoryPath.toString()));
        }
    }

    private void handlerMetadata(Artifactory artifactory, String repositoryName, RepositoryPath repositoryPath) {
        try {
            String artifactPath = RepositoryFiles.relativizePath(repositoryPath);
            if (!artifactPath.startsWith(java.io.File.separator)) {
                artifactPath = java.io.File.separator + artifactPath;
            }
            String metadataUrl = "/api/storage/" + repositoryName + artifactPath + "?properties";
            log.info("Get metadata url [{}]", metadataUrl);
            ArtifactoryRequest request = new ArtifactoryRequestImpl()
                    .method(ArtifactoryRequest.Method.GET)
                    .apiUrl(metadataUrl);
            ArtifactoryResponse response = artifactory.restCall(request);
            log.info("Get metadata [{}] response [{}]", repositoryPath.toString(), JSONObject.toJSONString(response.getStatusLine()));
            if (response.getStatusLine().getStatusCode() == HttpStatus.OK.value()) {
                String rawBody = response.getRawBody(), propertiesKey = "properties";
                if (StringUtils.isNotBlank(rawBody) && JSONUtil.isJson(rawBody)) {
                    JSONObject propertiesJson = JSONObject.parseObject(rawBody);
                    if (propertiesJson.containsKey(propertiesKey)) {
                        Set<String> propertiesSet = propertiesJson.getJSONObject(propertiesKey).keySet();
                        if (CollectionUtils.isNotEmpty(propertiesSet)) {
                            log.info("Get metadata [{}] successful! [{}]", repositoryPath.toString(), propertiesSet);
                            deleteMetadata(artifactory, repositoryName, repositoryPath, String.join(",", propertiesSet));
                        }
                    }
                }
            } else {
                log.warn("Get metadata [{}] failed. Response code [{}]", repositoryPath.toString(), response.getStatusLine().getStatusCode());
            }
        } catch (IOException e) {
            log.error(ExceptionUtils.getStackTrace(e));
        }
    }

    private void deleteMetadata(Artifactory artifactory, String repositoryName, RepositoryPath repositoryPath, String properties) {
        try {
            String artifactPath = RepositoryFiles.relativizePath(repositoryPath);
            if (!artifactPath.startsWith(java.io.File.separator)) {
                artifactPath = java.io.File.separator + artifactPath;
            }
            String metadataUrl = "/api/storage/" + repositoryName + artifactPath + "?properties=" + properties;
            log.info("Delete metadata url [{}]", metadataUrl);
            ArtifactoryRequest request = new ArtifactoryRequestImpl()
                    .method(ArtifactoryRequest.Method.DELETE)
                    .apiUrl(metadataUrl);
            ArtifactoryResponse response = artifactory.restCall(request);
            log.info("Delete metadata [{}] response [{}]", repositoryPath.toString(), JSONObject.toJSONString(response.getStatusLine()));
            if (response.getStatusLine().getStatusCode() == HttpStatus.NO_CONTENT.value()) {
                log.info("Delete metadata [{}] successful!", repositoryPath.toString());
            } else {
                log.warn("Delete metadata [{}] failed. Response code [{}]", repositoryPath.toString(), response.getStatusLine().getStatusCode());
            }
        } catch (IOException e) {
            log.error(ExceptionUtils.getStackTrace(e));
        }
    }

}
