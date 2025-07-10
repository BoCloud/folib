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
package com.folib.client;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.alibaba.fastjson.TypeReference;
import com.folib.forms.*;
import com.folib.vo.*;
import com.google.common.collect.Lists;
import com.folib.dto.ArtifactPromotion;
import com.folib.forms.*;
import com.folib.vo.*;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.http.HttpStatus;
import org.glassfish.jersey.media.multipart.Boundary;
import org.glassfish.jersey.media.multipart.FormDataMultiPart;
import org.glassfish.jersey.media.multipart.file.StreamDataBodyPart;
import org.glassfish.jersey.media.multipart.internal.MultiPartWriter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.ResponseEntity;
import org.springframework.util.CollectionUtils;

import javax.ws.rs.ServerErrorException;
import javax.ws.rs.client.Entity;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLEncoder;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.*;

/**
 * folib rest client
 *
 * @author veadan
 */
public class RestClient extends ArtifactClient {

    private static final Logger logger = LoggerFactory.getLogger(RestClient.class);

    public RestClient() {

    }

    /**
     * 获取客户端实例
     *
     * @param username 用户名
     * @param password 密码
     * @return 客户端实例
     */
    public static RestClient getRestClientInstance(String username, String password) {
        String host = System.getProperty("folib.host") != null ?
                System.getProperty("folib.host") :
                "localhost";
        int port = System.getProperty("folib.port") != null ?
                Integer.parseInt(System.getProperty("folib.port")) :
                38080;
        RestClient client = new RestClient();
        client.setUsername(username);
        client.setPassword(password);
        client.setPort(port);
        client.setContextBaseUrl("http://" + host + ":" + client.getPort());
        return client;
    }

    /**
     * 获取客户端实例
     *
     * @param baseUrl  地址
     * @param username 用户名
     * @param password 密码
     * @return 客户端实例
     */
    public static RestClient getRestClientInstance(String baseUrl, String username, String password) {

        try {
            URL url = new URL(baseUrl);
//            // 获取协议
//            String protocol = url.getProtocol();
//            // 获取主机名（域名或IP地址）
//            String host = url.getHost();
            // 获取端口号，如果没有明确指定端口，默认为协议的默认端口（如HTTP的默认端口是80）
            int port = url.getPort();
            if (port == -1) {
                port = url.getDefaultPort();
            }
            RestClient client = new RestClient();
            client.setUsername(username);
            client.setPassword(password);
            client.setPort(port);
            client.setContextBaseUrl(baseUrl);
            return client;

        } catch (MalformedURLException e) {
            throw new RuntimeException(e);
        }


    }

    /**
     * 返回错误信息
     *
     * @param response response
     */
    public static void displayResponseError(Response response) {
        logger.error("Status code {}", response.getStatus());
        logger.error("Status info {}", response.getStatusInfo().getReasonPhrase());
        logger.error("Response message {}", response.readEntity(String.class));
        logger.error(response.toString());
    }

    /**
     * 设置监听端口
     *
     * @param port 要监听的端口
     * @return 响应状态码
     */
    public int setListeningPort(int port) {
        String url = getContextBaseUrl() + "/api/configuration/folib/port/" + port;
        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);
        Response response = resource.request(MediaType.TEXT_PLAIN).put(Entity.entity(port, MediaType.TEXT_PLAIN));
        return response.getStatus();
    }


    /**
     * 获取正在监听的端口
     *
     * @return 正在监听的端口
     */
    public int getListeningPort() {
        String url = getContextBaseUrl() + "/api/configuration/folib/port";
        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);
        return resource.request(MediaType.TEXT_PLAIN).get(Integer.class);
    }

    /**
     * 设置服务基础路径
     *
     * @param baseUrl 服务基础路径
     * @return 响应状态码
     */
    public int setBaseUrl(String baseUrl) {
        String url = getContextBaseUrl() + "/api/configuration/folib/baseUrl/" + baseUrl;
        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);
        Response response = resource.request(MediaType.TEXT_PLAIN).put(Entity.entity(baseUrl, MediaType.TEXT_PLAIN));
        return response.getStatus();
    }

    /**
     * 获取服务基础路径
     *
     * @return 服务基础路径
     */
    public String getBaseUrl() {
        String url = getContextBaseUrl() + "/api/configuration/folib/baseUrl";
        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);
        return resource.request(MediaType.TEXT_PLAIN).get(String.class);
    }

    /**
     * 创建存储空间
     *
     * @param storage 存储空间信息
     * @return 响应状态码
     */
    public int addStorage(StorageForm storage) {
        String url = getContextBaseUrl() + "/api/configuration/folib/storages";
        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);
        Response response = resource.request(MediaType.APPLICATION_JSON_TYPE)
                .put(Entity.entity(storage, MediaType.APPLICATION_JSON_TYPE));
        return response.getStatus();
    }

    /**
     * 按照存储空间名称查询存储空间信息
     *
     * @param storageId 存储空间名称
     * @return 存储空间信息
     */
    public Storage getStorage(String storageId) {
        String url = getContextBaseUrl() + "/api/configuration/folib/storages/" + storageId;
        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);
        final Response response = resource.request(MediaType.APPLICATION_JSON).get();
        Storage storage = null;
        if (response.getStatus() == HttpStatus.SC_OK) {
            storage = response.readEntity(Storage.class);
        } else if (response.getStatus() == HttpStatus.SC_NOT_FOUND) {
            return null;
        } else {
            displayResponseError(response);
            throw new ServerErrorException(response.getStatus() + " | Unable to greet()",
                    Response.Status.INTERNAL_SERVER_ERROR);
        }
        return storage;
    }

    /**
     * 删除存储空间
     *
     * @param storageId 存储空间名称
     * @param force     强制删除 true 是 false 否
     * @return 响应状态码
     */
    public int deleteStorage(String storageId, boolean force) {
        String url = getContextBaseUrl() + "/api/configuration/folib/storages/" + storageId +
                (force ? "?force=true" : "");
        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);
        Response response = resource.request().delete();
        return response.getStatus();
    }

    /**
     * 创建仓库
     *
     * @param storageId      存储空间名称
     * @param repositoryForm 仓库信息
     * @return 响应状态码
     */
    public int addRepository(String storageId, RepositoryForm repositoryForm) {
        if (repositoryForm == null) {
            logger.error("Unable to add non-existing repository.");
            throw new ServerErrorException("Unable to add non-existing repository.",
                    Response.Status.INTERNAL_SERVER_ERROR);
        }
        WebTarget resource;
        if (storageId == null) {
            logger.error("Storage associated with repo is null.");
            throw new ServerErrorException("Storage associated with repo is null.",
                    Response.Status.INTERNAL_SERVER_ERROR);
        }
        try {
            String url = getContextBaseUrl() + "/api/configuration/folib/storages/" +
                    storageId + "/" + repositoryForm.getId();
            logger.info("Sending request to create repository " + url);
            resource = getClientInstance().target(url);
        } catch (RuntimeException e) {
            logger.error("Unable to create web resource.", e);
            throw new ServerErrorException(Response.Status.INTERNAL_SERVER_ERROR);
        }
        setupAuthentication(resource);
        Response response = resource.request(MediaType.APPLICATION_JSON)
                .put(Entity.entity(repositoryForm, MediaType.APPLICATION_JSON));
        return response.getStatus();
    }

    /**
     * 按照存储空间名称和仓库名称查询存储信息
     *
     * @param storageId    存储空间名称
     * @param repositoryId 仓库名称
     * @return 存储信息
     */
    public Repository getRepository(String storageId, String repositoryId) {
        String url = getContextBaseUrl() + "/api/configuration/folib/storages/" + storageId + "/" + repositoryId;
        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);
        final Response response = resource.request(MediaType.APPLICATION_JSON).get();
        Repository repository = null;
        if (response.getStatus() == HttpStatus.SC_OK) {
            repository = response.readEntity(Repository.class);
        } else {
            displayResponseError(response);
            throw new ServerErrorException(response.getStatus() + " | Unable to greet()",
                    Response.Status.INTERNAL_SERVER_ERROR);
        }
        return repository;
    }

    /**
     * 删除仓库
     *
     * @param storageId    存储空间名称
     * @param repositoryId 仓库名称
     * @param force        强制删除 true 是 false 否
     * @return 响应状态码
     */
    public int deleteRepository(String storageId, String repositoryId, boolean force) {
        String url = getContextBaseUrl() +
                "/api/configuration/folib/storages/" + storageId + "/" + repositoryId +
                ("?force=" + force);
        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);
        Response response = resource.request().accept(MediaType.WILDCARD).delete();
        return response.getStatus();
    }

    /**
     * 搜索制品
     *
     * @param query     关键词
     * @param mediaType mediaType
     * @return 结果
     * @throws UnsupportedEncodingException unsupportedEncodingException
     */
    public String search(String query, MediaType mediaType) throws UnsupportedEncodingException {
        return search(null, query, mediaType);
    }

    /**
     * 搜索制品
     *
     * @param repositoryId 仓库名称
     * @param query        关键词
     * @param mediaType    mediaType
     * @return 结果
     * @throws UnsupportedEncodingException unsupportedEncodingException
     */
    public String search(String repositoryId, String query, MediaType mediaType) throws UnsupportedEncodingException {
        String url = getContextBaseUrl() + "/api/search?" +
                (repositoryId != null ? "repositoryId=" + URLEncoder.encode(repositoryId, "UTF-8") : "") +
                "&q=" + URLEncoder.encode(query, "UTF-8");

        WebTarget webResource = getClientInstance().target(url);
        setupAuthentication(webResource);
        final Response response = webResource.request(mediaType).get();
        final String asText = response.readEntity(String.class);
        return asText;
    }

    /**
     * 重建maven metadata
     *
     * @param storageId    存储空间名称
     * @param repositoryId 仓库名称
     * @param basePath     路径
     * @return 响应状态码
     */
    public int rebuildMetadata(String storageId, String repositoryId, String basePath) {
        String url = getContextBaseUrl() + "/api/maven/metadata/" + storageId + "/" + repositoryId + "/" +
                (basePath != null ? basePath : "");
        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);
        Response response = resource.request(MediaType.TEXT_PLAIN).post(Entity.entity("Rebuild",
                MediaType.APPLICATION_XML));
        return response.getStatus();
    }

    /**
     * 从maven metadata 中删除版本信息
     *
     * @param storageId    存储空间名称
     * @param repositoryId 仓库名称
     * @param artifactPath 制品路径
     * @param version      版本
     * @param classifier   classifier
     * @param metadataType metadataType
     * @return 响应状态码
     */
    public int removeVersionFromMetadata(String storageId, String repositoryId, String artifactPath, String version, String classifier, String metadataType) {
        String url = getContextBaseUrl() + "/api/maven/metadata/" +
                storageId + "/" + repositoryId + "/" +
                (artifactPath != null ? artifactPath : "") +
                "?version=" + version + (classifier != null ? "&classifier=" + classifier : "") +
                "&metadataType=" + metadataType;
        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);
        Response response = resource.request().delete();
        return response.getStatus();
    }

    /**
     * 制品复制
     *
     * @param path             路径
     * @param srcStorageId     源存储空间名称
     * @param srcRepositoryId  源仓库名称
     * @param destStorageId    目标存储空间名称
     * @param destRepositoryId 目标仓库名称
     */
    public void copy(String path, String srcStorageId, String srcRepositoryId, String destStorageId, String destRepositoryId) {
        String url = getContextBaseUrl() + "/storages/copy/" + path +
                "?srcStorageId=" + srcStorageId +
                "&srcRepositoryId=" + srcRepositoryId +
                "&destStorageId=" + destStorageId +
                "&destRepositoryId=" + destRepositoryId;
        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);
        resource.request(MediaType.TEXT_PLAIN).post(Entity.entity("Copy", MediaType.TEXT_PLAIN));
    }

    /**
     * 生成token
     *
     * @return token
     */
    public String generateUserSecurityToken() {
        String url = String.format("%s/api/users/%s/generate-security-token", getContextBaseUrl(), getUsername());
        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);
        Response response = resource.request(MediaType.TEXT_PLAIN).get();
        if (response.getStatus() != HttpStatus.SC_OK) {
            displayResponseError(response);
            throw new ServerErrorException(response.getEntity() + " | Unable to get Security Token",
                    response.getStatus());
        } else {
            return response.readEntity(String.class);
        }
    }


    /**
     * 按照存储空间名称和仓库类型查询存储空间下的仓库列表
     *
     * @param storageId      存储空间名称
     * @param repositoryType 仓库类型 hosted 本地库 proxy 代理库 group 组合库 all 所有仓库类型
     * @return 存储空间下的仓库列表
     */
    public List<Repository> getRepositories(String storageId, String repositoryType) {
        String url = getContextBaseUrl() + "/api/configuration/folib/storages/repositories/%s/%s";
        url = String.format(url, storageId, repositoryType);
        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);
        Response response = resource.request(MediaType.APPLICATION_JSON).get();
        if (response.getStatus() != HttpStatus.SC_OK) {
            displayResponseError(response);
            throw new ServerErrorException(response.getStatus() + " | Unable to greet()",
                    Response.Status.INTERNAL_SERVER_ERROR);
        } else {
            String res = response.readEntity(String.class);
            if (StringUtils.isNotBlank(res)) {
                return JSONArray.parseArray(res, Repository.class);
            } else {
                return Collections.emptyList();
            }
        }
    }

    /**
     * 按照存储空间名称和仓库名称获取仓库下的文件列表
     *
     * @param storageId    存储空间名称
     * @param repositoryId 仓库名称
     * @param path         路径
     * @return 仓库下的文件列表
     */
    public List<Folder> folders(String storageId, String repositoryId, String path) {
        Repository repository = getRepository(storageId, repositoryId);
        String layout = repository.getLayout();
        String url = getContextBaseUrl() + "/api/browse/%s/%s/%s";
        url = String.format(url, storageId, repositoryId, path);
        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);
        Response response = resource.request(MediaType.APPLICATION_JSON).get();
        if (response.getStatus() != HttpStatus.SC_OK) {
            displayResponseError(response);
            throw new ServerErrorException(response.getStatus() + " | Unable to greet()",
                    Response.Status.INTERNAL_SERVER_ERROR);
        } else {
            String res = response.readEntity(String.class);
            if (StringUtils.isNotBlank(res)) {
                List<Folder> folders = Lists.newArrayList(), directories, files, dockerFiles;
                JSONObject folderJson = JSONObject.parseObject(res);
                String directoriesKey = "directories", filesKey = "files";
                if (folderJson.containsKey(directoriesKey) && StringUtils.isNotBlank(folderJson.getString(directoriesKey))) {
                    directories = JSONArray.parseArray(folderJson.getString(directoriesKey), Folder.class);
                    if (!CollectionUtils.isEmpty(directories)) {
                        directories.forEach(item -> item.setFolder(true));
                        folders.addAll(directories);
                    }
                }
                if (folderJson.containsKey(filesKey) && StringUtils.isNotBlank(folderJson.getString(filesKey))) {
                    files = JSONArray.parseArray(folderJson.getString(filesKey), Folder.class);
                    files.forEach(item -> item.setFolder(false));
                    folders.addAll(files);
                }
                return folders;
            } else {
                return Collections.emptyList();
            }
        }
    }

    /**
     * 获取制品信息
     *
     * @param storageId    存储空间名称
     * @param repositoryId 仓库名称
     * @param path         路径
     * @return 制品信息
     */
    public ArtifactInfo getArtifactInfo(String storageId, String repositoryId, String path) {
        String url = getContextBaseUrl() + "/api/browse/getArtifact/%s/%s/%s";
        url = String.format(url, storageId, repositoryId, path);
        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);
        Response response = resource.request(MediaType.APPLICATION_JSON).get();
        if (response.getStatus() != HttpStatus.SC_OK) {
            displayResponseError(response);
            throw new ServerErrorException(response.getStatus() + " | Unable to greet()",
                    Response.Status.INTERNAL_SERVER_ERROR);
        } else {
            return response.readEntity(ArtifactInfo.class);
        }
    }

    /**
     * 搜索制品
     *
     * @param searchArtifact 搜索参数
     * @return 搜索结果
     */
    public SearchArtifactPage searchArtifactPage(SearchArtifact searchArtifact) {
        String url = getContextBaseUrl() + "/api/fql";
        if (Boolean.TRUE.equals(searchArtifact.getRegex()) && StringUtils.isNotBlank(searchArtifact.getArtifactName())) {
            //开启正则
            if (StringUtils.isNotBlank(searchArtifact.getPattern())) {
                searchArtifact.setArtifactName(searchArtifact.getPattern());
            } else {
                String regex = "(%s)(.*%s.*)";
                String prefix = searchArtifact.getStorageId();
                if (StringUtils.isNotBlank(searchArtifact.getRepositoryId())) {
                    prefix = prefix + "-" + searchArtifact.getRepositoryId();
                }
                regex = String.format(regex, prefix, searchArtifact.getArtifactName());
                searchArtifact.setArtifactName(regex);
            }
        }
        Map<String, String> paramsMap = JSON.parseObject(JSON.toJSONString(searchArtifact), new TypeReference<Map<String, String>>() {
        });
        String params = createLinkStringByGet(paramsMap);
        url = url + params;
        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);
        Response response = resource.request(MediaType.APPLICATION_JSON).get();
        if (response.getStatus() != HttpStatus.SC_OK) {
            displayResponseError(response);
            throw new ServerErrorException(response.getStatus() + " | Unable to greet()",
                    Response.Status.INTERNAL_SERVER_ERROR);
        } else {
            SearchArtifactPage searchArtifactPage = SearchArtifactPage.builder().total(0L).build();
            String res = response.readEntity(String.class);
            if (StringUtils.isNotBlank(res)) {
                JSONObject searchJson = JSONObject.parseObject(res);
                String artifactKey = "artifact";
                if (searchJson.containsKey(artifactKey) && StringUtils.isNotBlank(searchJson.getString(artifactKey))) {
                    List<SearchArtifactInfo> searchArtifactInfoList = JSONArray.parseArray(searchJson.getString(artifactKey), SearchArtifactInfo.class);
                    searchArtifactPage.setArtifactInfoList(searchArtifactInfoList);
                }
                searchArtifactPage.setTotal(searchJson.getLongValue("total"));
                return searchArtifactPage;
            }
            return searchArtifactPage;
        }
    }

    /**
     * 获取制品漏洞报告信息
     *
     * @param storageId    存储空间名称
     * @param repositoryId 仓库名称
     * @param path         路径
     * @return 制品漏洞报告信息
     */
    public VulnerabilityReport vulnerabilityReport(String storageId, String repositoryId, String path) {
        ArtifactInfo artifactInfo = getArtifactInfo(storageId, repositoryId, path);
        if (Objects.nonNull(artifactInfo) && Objects.nonNull(artifactInfo.getArtifact())) {
            Artifact artifact = artifactInfo.getArtifact();
            VulnerabilityReport vulnerabilityReport = VulnerabilityReport.builder().path(artifact.getArtifactPath())
                    .storage(artifact.getStorageId()).repository(artifact.getRepositoryId()).critical(artifact.getCriticalVulnerabilitiesCount())
                    .high(artifact.getHighVulnerabilitiesCount()).medium(artifact.getMediumVulnerabilitiesCount()).low(artifact.getLowVulnerabilitiesCount())
                    .vulnerabilitiesCount(artifact.getVulnerabilitiesCount()).build();
            String report = artifact.getReport();
            if (StringUtils.isNotBlank(report)) {
                vulnerabilityReport.setDependencies(JSONObject.parseArray(report));
            }
            Boolean scanComplete = "scanComplete".equals(artifact.getSafeLevel());
            vulnerabilityReport.setScanComplete(scanComplete);
            return vulnerabilityReport;
        }
        return null;
    }

    /**
     * 制品复制操作
     *
     * @param artifactPromotion 制品复制参数
     * @return ResponseEntity 响应实体
     */
    public ResponseEntity copy(ArtifactPromotion artifactPromotion) {
        String url = getContextBaseUrl() + "/api/artifact/folib/promotion/copy";
        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);
        Response response = resource.request(MediaType.APPLICATION_JSON).
                post(Entity.entity(artifactPromotion, MediaType.APPLICATION_JSON));
        if (response.getStatus() != HttpStatus.SC_OK) {
            displayResponseError(response);
            throw new ServerErrorException(response.getStatus() + " | Unable to greet()",
                    Response.Status.INTERNAL_SERVER_ERROR);
        } else {
            return ResponseEntity.ok("Artifact copying");
        }
    }

    /**
     * 制品移动操作
     *
     * @param artifactPromotion 制品移动参数
     * @return ResponseEntity 响应实体
     */
    public ResponseEntity move(ArtifactPromotion artifactPromotion) {
        String url = getContextBaseUrl() + "/api/artifact/folib/promotion/move";
        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);
        Response response = resource.request(MediaType.APPLICATION_JSON).
                post(Entity.entity(artifactPromotion, MediaType.APPLICATION_JSON));
        if (response.getStatus() != HttpStatus.SC_OK) {
            displayResponseError(response);
            throw new ServerErrorException(response.getStatus() + " | Unable to greet()",
                    Response.Status.INTERNAL_SERVER_ERROR);
        } else {
            return ResponseEntity.ok("Artifact moving");
        }
    }

    /**
     * 上传制品
     *
     * @param uploadArtifactFrom 上传参数
     * @return ResponseEntity 响应实体
     */
    public ResponseEntity upload(UploadArtifactFrom uploadArtifactFrom) {
        String url = getContextBaseUrl() + "/api/artifact/folib/promotion/upload-files";
        FormDataMultiPart part = new FormDataMultiPart();
        part.field("storageId", uploadArtifactFrom.getStorageId());
        part.field("repostoryId", uploadArtifactFrom.getRepostoryId());
        part.field("filePathMap", uploadArtifactFrom.getFilePathMap());
        part.field("fileMetaDataMap", uploadArtifactFrom.getFileMetaDataMap());
        try (InputStream is = uploadArtifactFrom.getFiles()[0].getInputStream()) {
            part.bodyPart(new StreamDataBodyPart("files", is,
                    uploadArtifactFrom.getFiles()[0].getOriginalFilename()));
            WebTarget resource = getClientInstance().register(MultiPartWriter.class).target(url);
            setupAuthentication(resource);
            Response response = resource.request(MediaType.APPLICATION_JSON).header("Mime-Version", "1.0").
                    post(Entity.entity(part, Boundary.addBoundary(MediaType.MULTIPART_FORM_DATA_TYPE)));
            if (response.getStatus() != HttpStatus.SC_OK) {
                displayResponseError(response);
                throw new ServerErrorException(response.getStatus() + " | Unable to greet()",
                        Response.Status.INTERNAL_SERVER_ERROR);
            } else {
                return ResponseEntity.ok("上传成功");
            }
        } catch (IOException e) {
            logger.error("Artifact upload error {}", e.getMessage());
            return ResponseEntity.status(org.springframework.http.HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(e.getMessage());
        }
    }

    /**
     * 上传制品
     *
     * @param uploadArtifactFrom 上传参数
     * @return ResponseEntity 响应实体
     */
    public String uploadFile(UploadArtifactFrom uploadArtifactFrom) {
        String url = getContextBaseUrl() + "/api/artifact/folib/promotion/upload-files";
        FormDataMultiPart part = new FormDataMultiPart();
        part.field("storageId", uploadArtifactFrom.getStorageId());
        part.field("repostoryId", uploadArtifactFrom.getRepostoryId());
        part.field("filePathMap", uploadArtifactFrom.getFilePathMap());
        part.field("fileMetaDataMap", uploadArtifactFrom.getFileMetaDataMap());
        File file = uploadArtifactFrom.getFile();
        try (InputStream is = Files.newInputStream(Path.of(file.getAbsolutePath()))) {
            part.bodyPart(new StreamDataBodyPart("files", is, file.getName()));
            WebTarget resource = getClientInstance().register(MultiPartWriter.class).target(url);
            setupAuthentication(resource);
            Response response = resource.request(MediaType.APPLICATION_JSON).header("Mime-Version", "1.0").
                    post(Entity.entity(part, Boundary.addBoundary(MediaType.MULTIPART_FORM_DATA_TYPE)));
            if (response.getStatus() != HttpStatus.SC_OK) {
                displayResponseError(response);
                throw new ServerErrorException(response.getStatus() + " | Unable to greet()",
                        Response.Status.INTERNAL_SERVER_ERROR);
            } else {
                return "success";
            }
        } catch (IOException e) {
            logger.error("Artifact upload error {}", e.getMessage());
            return "error";
        }
    }

    /**
     * 制品节点晋级
     *
     * @param promotionNodeOption 晋级参数
     * @return ResponseEntity  响应实体
     */
    public ResponseEntity artifactPromotion(PromotionNodeOption promotionNodeOption) {
        String url = getContextBaseUrl() + "/api/artifact/folib/promotion/nodeOption";
        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);
        Response response = resource.request().
                post(Entity.entity(promotionNodeOption, MediaType.APPLICATION_JSON));
        if (response.getStatus() != HttpStatus.SC_OK) {
            displayResponseError(response);
            throw new ServerErrorException(response.getStatus() + " | Unable to greet()",
                    Response.Status.INTERNAL_SERVER_ERROR);
        } else {
            return ResponseEntity.ok("晋级成功");
        }
    }

    /**
     * 添加制品元数据
     *
     * @param artifactMetadataForm 制品元数据实体对象
     * @return ResponseEntity 响应实体
     */
    public ResponseEntity createArtifactMetadata(ArtifactMetadataForm artifactMetadataForm) {
        String url = getContextBaseUrl() + "/api/artifact/artifactMetadata";
        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);
        Response response = resource.request().
                put(Entity.entity(artifactMetadataForm, MediaType.APPLICATION_JSON));
        if (response.getStatus() != HttpStatus.SC_OK) {
            displayResponseError(response);
            throw new ServerErrorException(response.getStatus() + " | Unable to greet()",
                    Response.Status.INTERNAL_SERVER_ERROR);
        } else {
            return ResponseEntity.ok("制品元数据创建成功");
        }

    }

    /**
     * 更新制品元数据
     *
     * @param artifactMetadataForm 制品元数据实体对象
     * @return ResponseEntity 响应实体
     */
    public ResponseEntity updateArtifactMetadata(ArtifactMetadataForm artifactMetadataForm) {
        String url = getContextBaseUrl() + "/api/artifact/artifactMetadata";
        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);
        Response response = resource.request().
                post(Entity.entity(artifactMetadataForm, MediaType.APPLICATION_JSON));
        if (response.getStatus() != HttpStatus.SC_OK) {
            displayResponseError(response);
            throw new ServerErrorException(response.getStatus() + " | Unable to greet()",
                    Response.Status.INTERNAL_SERVER_ERROR);
        } else {
            return ResponseEntity.ok("制品元数据更新成功");
        }
    }

    /**
     * 删除制品元数据
     *
     * @param artifactMetadataForm 制品元数据实体对象
     * @return ResponseEntity 响应实体
     */
    public ResponseEntity deleteArtifactMetadata(ArtifactMetadataForm artifactMetadataForm) {
        String url = getContextBaseUrl() + "/api/artifact/deleteArtifactMetadata";
        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);
        Response response = resource.request().post(Entity.entity(artifactMetadataForm, MediaType.APPLICATION_JSON));
        if (response.getStatus() != HttpStatus.SC_OK) {
            displayResponseError(response);
            throw new ServerErrorException(response.getStatus() + " | Unable to greet()",
                    Response.Status.INTERNAL_SERVER_ERROR);
        } else {
            return ResponseEntity.ok("制品元数据删除成功");
        }
    }

    /**
     * 批量新增制品元数据
     *
     * @param artifactMetadataBatchForm 制品元数据实体对象
     * @return ResponseEntity 响应实体
     */
    public ResponseEntity addBatchArtifactMetadata(ArtifactMetadataBatchForm artifactMetadataBatchForm) {
        String url = getContextBaseUrl() + "/api/artifact/batchArtifactMetadata";
        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);
        Response response = resource.request().
                post(Entity.entity(artifactMetadataBatchForm.getList(), MediaType.APPLICATION_JSON));
        if (response.getStatus() != HttpStatus.SC_OK) {
            displayResponseError(response);
            throw new ServerErrorException(response.getStatus() + " | Unable to greet()",
                    Response.Status.INTERNAL_SERVER_ERROR);
        } else {
            return ResponseEntity.ok("批量新增制品元数据成功");
        }
    }

    public WebTarget prepareTarget(String arg) {
        return setupAuthentication(prepareUnauthenticatedTarget(arg));
    }

    public WebTarget prepareUnauthenticatedTarget(String arg) {
        String url = getContextBaseUrl() + arg;
        logger.info("Prepare target URL {}", url);
        return getClientInstance().target(url);
    }

    public WebTarget prepareTarget(String arg, String username, String password) {
        this.username = username;
        this.password = password;
        return prepareTarget(arg);
    }

    /**
     * 仓库漏洞信息统计
     *
     * @param storageId    存储空间名称
     * @param repositoryId 仓库名称
     * @return 仓库漏洞信息统计
     */
    public RepositoryVulnerabilityStatistics vulnerabilityStatistics(String storageId, String repositoryId) {
        String params = "?storageId=%s&repositoryId=%s";
        params = String.format(params, storageId, repositoryId);
        String url = getContextBaseUrl() + "/api/vulnerability/repositoryVulnerabilityStatistics" + params;
        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);
        Response response = resource.request(MediaType.APPLICATION_JSON).get();
        if (response.getStatus() != HttpStatus.SC_OK) {
            displayResponseError(response);
            throw new ServerErrorException(response.getStatus() + " | Unable to greet()",
                    Response.Status.INTERNAL_SERVER_ERROR);
        } else {
            String data = response.readEntity(String.class);
            if (StringUtils.isNotBlank(data)) {
                JSONObject dataJson = JSONObject.parseObject(data);
                return JSONObject.toJavaObject(dataJson, RepositoryVulnerabilityStatistics.class);
            }
            return null;
        }
    }

    /**
     * 漏洞分页查询
     *
     * @param vulnerabilityPage 漏洞查询参数
     * @return 漏洞分页结果
     */
    public Page<Vulnerability> vulnerabilityPage(VulnerabilityPage vulnerabilityPage) {
        String url = getContextBaseUrl() + "/api/vulnerability/page";
        Map<String, String> paramsMap = JSON.parseObject(JSON.toJSONString(vulnerabilityPage), new TypeReference<Map<String, String>>() {
        });
        if (Objects.isNull(vulnerabilityPage.getSource())) {
            vulnerabilityPage.setSource(2);
        }
        String params = createLinkStringByGet(paramsMap);
        url = url + params;
        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);
        Response response = resource.request(MediaType.APPLICATION_JSON).get();
        if (response.getStatus() != HttpStatus.SC_OK) {
            displayResponseError(response);
            throw new ServerErrorException(response.getStatus() + " | Unable to greet()",
                    Response.Status.INTERNAL_SERVER_ERROR);
        } else {
            Long total = 0L;
            String res = response.readEntity(String.class);
            List<Vulnerability> list = null;
            if (StringUtils.isNotBlank(res)) {
                JSONObject pageJson = JSONObject.parseObject(res);
                String dataKey = "data", rowsKey = "rows";
                if (pageJson.containsKey(dataKey) && StringUtils.isNotBlank(pageJson.getString(dataKey))) {
                    JSONObject dataJson = pageJson.getJSONObject(dataKey);
                    if (dataJson.containsKey(rowsKey) && StringUtils.isNotBlank(dataJson.getString(rowsKey))) {
                        list = JSONArray.parseArray(dataJson.getString(rowsKey), Vulnerability.class);
                    }
                    total = dataJson.getLongValue("total");
                }
                return new Page<Vulnerability>(list, total);
            }
            return new Page<Vulnerability>(null, total);
        }
    }

    /**
     * 漏洞图谱数据
     *
     * @param uuid         漏洞id
     * @param storageId    存储空间名称
     * @param repositoryId 仓库名称
     * @return 漏洞分页结果
     */
    public VulnerabilityGraph vulnerabilityGraph(String uuid, String storageId, String repositoryId) {
        String params = "?uuid=%s&storageId=%s&repositoryId=%s";
        params = String.format(params, uuid, storageId, repositoryId);
        String url = getContextBaseUrl() + "/api/vulnerability/graph";
        url = url + params;
        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);
        Response response = resource.request(MediaType.APPLICATION_JSON).get();
        if (response.getStatus() != HttpStatus.SC_OK) {
            displayResponseError(response);
            throw new ServerErrorException(response.getStatus() + " | Unable to greet()",
                    Response.Status.INTERNAL_SERVER_ERROR);
        } else {
            String res = response.readEntity(String.class);
            if (StringUtils.isNotBlank(res)) {
                res = res.replaceAll("\"name\"", "\"categoryName\"");
                res = res.replaceAll("\"count\"", "\"downloadCount\"");
                res = res.replaceAll("\"currency\"", "\"highestSeverityText\"");
                res = res.replaceAll("\"variableName\"", "\"versionEndExcluding\"");
                res = res.replaceAll("\"label\"", "\"name\"");
                return JSONObject.parseObject(res, VulnerabilityGraph.class);
            }
            return null;
        }
    }

    /**
     * 制品统计信息
     *
     * @return 制品统计信息
     */
    public ArtifactStatistics artifactStatistics() {
        String url = getContextBaseUrl() + "/api/artifact/artifactStatistics";
        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);
        Response response = resource.request(MediaType.APPLICATION_JSON).get();
        if (response.getStatus() != HttpStatus.SC_OK) {
            displayResponseError(response);
            throw new ServerErrorException(response.getStatus() + " | Unable to greet()",
                    Response.Status.INTERNAL_SERVER_ERROR);
        } else {
            String res = response.readEntity(String.class);
            if (StringUtils.isNotBlank(res)) {
                return JSONObject.parseObject(res, ArtifactStatistics.class);
            }
            Long zero = 0L;
            return ArtifactStatistics.builder().artifactsCount(zero).artifactsNormalCount(zero).artifactsVulnerabilitiesCount(zero)
                    .criticalVulnerabilitiesCount(zero).highVulnerabilitiesCount(zero).lowVulnerabilitiesCount(zero)
                    .mediumVulnerabilitiesCount(zero).suppressedVulnerabilitiesCount(zero).vulnerabilitiesCount(zero).artifactsBytes(zero).build();
        }
    }

    /**
     * 获取批量下载制品路径
     *
     * @param resolveBatchPathReq 参数
     * @return ResponseEntity 响应实体
     */
    public List<BatchDownload> getResolveBatchPath(ResolveBatchPath resolveBatchPathReq) {
        String url = getContextBaseUrl() + "/artifactory/resolveBatchPath";
        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);
        Response response = resource.request().
                post(Entity.entity(resolveBatchPathReq, MediaType.APPLICATION_JSON));
        if (response.getStatus() != HttpStatus.SC_OK) {
            displayResponseError(response);
            throw new ServerErrorException(response.getStatus() + " | Unable to greet()",
                    Response.Status.INTERNAL_SERVER_ERROR);
        } else {
            List<BatchDownload> batchDownloadList = Lists.newArrayList();
            String res = response.readEntity(String.class);
            if (StringUtils.isNotBlank(res)) {
                List<BatchDownload> resList = JSONArray.parseArray(res, BatchDownload.class);
                if (!CollectionUtils.isEmpty(resList)) {
                    batchDownloadList = resList;
                }
            }
            return batchDownloadList;
        }
    }

    /**
     * 下载制品到文件
     *
     * @param url    下载路径
     * @param target 存储路径
     * @return File 文件
     */
    public File download(String url, String target) {
        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);
        Response response = resource.request().get();
        if (response.getStatus() != HttpStatus.SC_OK) {
            displayResponseError(response);
            throw new ServerErrorException(response.getStatus() + " | Unable to greet()",
                    Response.Status.INTERNAL_SERVER_ERROR);
        } else {
            try (InputStream inputStream = response.readEntity(InputStream.class)) {
                File file = new File(target);
                Files.copy(inputStream, Path.of(file.getAbsolutePath()), StandardCopyOption.REPLACE_EXISTING);
                return file;
            } catch (IOException ex) {
                logger.error(ExceptionUtils.getStackTrace(ex));
                throw new RuntimeException(ex.getMessage());
            }
        }
    }

    /**
     * 把数组所有元素排序，并按照“参数=参数值”的模式用“&”字符拼接成字符串
     *
     * @param params 需要排序并参与字符拼接的参数组
     * @return 拼接后字符串
     */
    private static String createLinkStringByGet(Map<String, String> params) {
        String preStr = "?";
        try {
            List<String> keys = new ArrayList<String>(params.keySet());
            Collections.sort(keys);
            for (int i = 0; i < keys.size(); i++) {
                String key = keys.get(i);
                String value = params.get(key);
                value = URLEncoder.encode(value, "UTF-8");
                if (i == keys.size() - 1) {
                    //拼接时，不包括最后一个&字符
                    preStr = preStr + key + "=" + value;
                } else {
                    preStr = preStr + key + "=" + value + "&";
                }
            }
        } catch (Exception ex) {
            logger.error("字符拼接错误：{}", ExceptionUtils.getStackTrace(ex));
        }
        return preStr;
    }

}
