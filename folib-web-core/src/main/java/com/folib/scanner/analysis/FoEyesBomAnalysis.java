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
package com.folib.scanner.analysis;

import cn.hutool.http.HttpStatus;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.google.common.collect.Maps;
import com.folib.components.thirdparty.foeyes.FoEyesProperties;
import com.folib.components.thirdparty.foeyes.enums.ClassifierEnum;
import com.folib.components.thirdparty.foeyes.enums.EndpointsEnum;
import com.folib.components.thirdparty.foeyes.reponse.ProjectInfo;
import com.folib.components.thirdparty.foeyes.request.CreateProjectRequest;
import com.folib.components.thirdparty.foeyes.request.ParentRequest;
import com.folib.constant.GlobalConstants;
import com.folib.domain.Artifact;
import com.folib.domain.FoEyesConfig;
import com.folib.enums.SafeLevelEnum;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.scanner.vulnerability.HttpClientPool;
import com.folib.services.ArtifactService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.http.HttpEntity;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpUriRequest;
import org.apache.http.client.methods.RequestBuilder;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.entity.mime.MultipartEntityBuilder;
import org.apache.http.util.EntityUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;

import javax.inject.Inject;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Slf4j
@Service(AnalysisConstant.FOEYES)
public class FoEyesBomAnalysis implements BomAnalysis {

    @Inject
    @Lazy
    private ArtifactService artifactService;

    @Autowired
    private FoEyesProperties foEyesProperties;
    @Inject
    @Lazy
    private RepositoryPathResolver repositoryPathResolver;


    /**
     * 分析bom CycloneDXJson
     *
     * @param taskName 任务名称
     * @param filePath bom 文件路径
     */
    @Override
    public void analysisCycloneDXJson(String taskName, RepositoryPath filePath) {

        if (foEyesProperties.getEnable()) {
            CompletableFuture.runAsync(() -> {
                try {
                    bomUpload(taskName, filePath);
                } catch (Exception e) {
                    log.error("taskName:{} analysisCycloneDXJson分析异常:{}", taskName, ExceptionUtils.getStackTrace(e));
                    throw new RuntimeException(e);
                }
            }).handle((result, throwable) -> {
                if (throwable != null) {
                    Optional<Artifact> optionalArtifact = null;
                    try {
                        optionalArtifact = Optional.ofNullable(getArtifact(taskName));
                    } catch (IOException e) {
                        throw new RuntimeException(e);
                    }
                    if (optionalArtifact.isPresent()) {
                        optionalArtifact.get().setSafeLevel(SafeLevelEnum.SCAN_FAIL.getLevel());
                        artifactService.saveOrUpdateArtifact(optionalArtifact.get());
                    }
                    log.error("Async task failed with exception", throwable);
                } else {
                    log.info("taskName:{} analysisCycloneDXJson分析完成", taskName);
                }
                return null;
            });
        }
    }

    public void bomUpload(String taskName, RepositoryPath bomRepositoryPath) {
        String parentProjectName = String.format("%s/%s", bomRepositoryPath.getStorageId(), bomRepositoryPath.getRepositoryId());
        ProjectInfo parentProjectInfo = queryProject(parentProjectName);
        if (Objects.isNull(parentProjectInfo)) {
            throw new RuntimeException(String.format("repositoryPath [%s] 未找到对应的父项目信息", bomRepositoryPath.getPath()));
        }
        String name = taskName;
        ProjectInfo projectInfo = createProject(CreateProjectRequest.builder().name(name).classifier(ClassifierEnum.FILE.getType()).parent(ParentRequest.builder().uuid(parentProjectInfo.getUuid()).build()).build());
        if (Objects.isNull(projectInfo)) {
            throw new RuntimeException(String.format("repositoryPath [%s] 未找到对应的项目信息", taskName));
        }
        String path = EndpointsEnum.BOM_UPLOAD.getPath();
        String url = getUrl(path);

        try (InputStream is = Files.newInputStream(bomRepositoryPath)) {
            HttpPost post = new HttpPost(url);
            post.setHeader("x-api-key", foEyesProperties.getAccessKey());
            MultipartEntityBuilder builder = MultipartEntityBuilder.create();
            builder.setContentType(ContentType.MULTIPART_FORM_DATA);
            builder.addTextBody("project", projectInfo.getUuid(), ContentType.TEXT_PLAIN);
            builder.addBinaryBody("multiPart", is, ContentType.APPLICATION_OCTET_STREAM, bomRepositoryPath.getFileName().toString());
            HttpEntity entity = builder.build();
            post.setEntity(entity);
            log.info("FoEyes bom upload url [{}]", url);
            try (final CloseableHttpResponse response = HttpClientPool.getClient().execute(post)) {
                if (response.getStatusLine().getStatusCode() == HttpStatus.HTTP_OK) {
                    log.info("FoEyes bom upload success url [{}] response [{}]", url, response);
                } else {
                    log.error("FoEyes bom upload fail url [{}] response [{}]", url, response);
                }
            }
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException(ex.getMessage());
        }
    }

    /**
     * 创建项目
     *
     * @param createProjectRequest 参数
     * @return 项目信息
     */
    public ProjectInfo createProject(CreateProjectRequest createProjectRequest) {
        ProjectInfo projectInfo = queryProject(createProjectRequest.getName());
        if (Objects.nonNull(projectInfo) && StringUtils.isNotBlank(projectInfo.getUuid())) {
            log.info("A project with the specified name [{}} already exists.", createProjectRequest.getName());
            return projectInfo;
        }
        String path = EndpointsEnum.CREATE_PROJECT.getPath();
        String url = getUrl(path);
        log.info("FoEyes create project url [{}]", url);
        Map<String, String> headers = commonsHeaders();
        headers.put("Content-Type", "application/json");
        createProjectRequest.setActive(true);
        String body = JSON.toJSONString(createProjectRequest);
        log.info("FoEyes create project url [{}] params [{}] ", url, body);
        HttpUriRequest httpPut = RequestBuilder.put()
                .setUri(url)
                .setHeader("Content-Type", "application/json")
                .addHeader("x-api-key", foEyesProperties.getAccessKey())
                .setEntity(new StringEntity(body, ContentType.APPLICATION_JSON))
                .build();
        try (final CloseableHttpResponse response = HttpClientPool.getClient().execute(httpPut)) {
            if (HttpStatus.HTTP_CREATED != response.getStatusLine().getStatusCode()) {
                log.error("FoEyes create project url [{}] params [{}] response [{}]", url, body, response);
                throw new RuntimeException(String.format("项目[%s]创建失败", createProjectRequest.getName()));
            }
            HttpEntity responseEntity = response.getEntity();
            String result = EntityUtils.toString(responseEntity);
            projectInfo = JSONObject.parseObject(result, ProjectInfo.class);
            log.info("FoEyes create project url [{}] params [{}] response [{}]", url, body, projectInfo);
            return projectInfo;
        } catch (IOException e) {
            log.error("FoEyes create project url [{}] params [{}]", url, body);
        }

        return projectInfo;
    }

    /**
     * 查询项目
     *
     * @param name 项目名称
     */
    public ProjectInfo queryProject(String name) {
        String path = EndpointsEnum.LOOKUP_PROJECT.getPath();
        String url = getUrl(path);
        log.info("FoEyes query project url [{}]", url);
        HttpUriRequest request = RequestBuilder.get()
                .setUri(url)
                .addParameter("name", name)
                .addParameter("version", "")
                .addHeader("x-api-key", foEyesProperties.getAccessKey())
                .build();

        ProjectInfo projectInfo = null;
        try (final CloseableHttpResponse response = HttpClientPool.getClient().execute(request)) {
            if (HttpStatus.HTTP_NOT_FOUND == response.getStatusLine().getStatusCode()) {
                log.info("FoEyes query project url [{}] params [{}] response [{}]", url, request.getParams(), response.getEntity());
                return null;
            }
            if (HttpStatus.HTTP_OK != response.getStatusLine().getStatusCode()) {
                throw new RuntimeException(String.format("查询[%s]项目信息失败", name));
            }
            HttpEntity responseEntity = response.getEntity();
            String result = EntityUtils.toString(responseEntity);
            projectInfo = JSONObject.parseObject(result, ProjectInfo.class);
            log.info("FoEyes query project url [{}] params [{}] response [{}]", url, request.getParams(), projectInfo);
            return projectInfo;
        } catch (IOException e) {
            log.error("查询项目信息异常", e);
        }
        return projectInfo;
    }

    public boolean enable() {
        return foEyesProperties.getEnable();
    }

    public FoEyesConfig getConfig() {
        return FoEyesConfig.builder().enable(enable()).accessKey(foEyesProperties.getAccessKey()).build();
    }

    private String getUrl(String path) {
        String prefixUrl = StringUtils.removeEnd(foEyesProperties.getBaseUrl(), GlobalConstants.SEPARATOR);
        if (!path.startsWith(GlobalConstants.SEPARATOR)) {
            path = GlobalConstants.SEPARATOR + path;
        }
        return prefixUrl + path;
    }

    private Map<String, String> commonsHeaders() {
        Map<String, String> headers = Maps.newHashMap();
        headers.put("x-api-key", foEyesProperties.getAccessKey());
        return headers;
    }

    public Artifact getArtifact(String bomId) throws IOException {

        String regex = "^(?<storageId>[^:]+):(?<repositoryId>[^:]+):(?<path>.+)$";
        Pattern pattern = Pattern.compile(regex);
        Matcher matcher = pattern.matcher(bomId);
        String storageId = "", repositoryId = "";
        String path = null;
        if (matcher.matches()) {
            storageId = matcher.group("storageId");
            repositoryId = matcher.group("repositoryId");
            path = matcher.group("path");
        } else {
            throw new RuntimeException("sbom artifact UUID:" + bomId + "is not exist");
        }
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
        return artifactService.findArtifact(repositoryPath, false);
    }
}
