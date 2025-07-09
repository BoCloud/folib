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
package com.folib.components.thirdparty.foeyes;

import cn.hutool.core.io.FileUtil;
import cn.hutool.http.HttpStatus;
import cn.hutool.http.Method;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.google.common.collect.Maps;
import com.folib.components.DistributedCacheComponent;
import com.folib.components.thirdparty.client.ThirdPartyClientComponent;
import com.folib.components.thirdparty.client.domain.ServerResponse;
import com.folib.components.thirdparty.foeyes.enums.ClassifierEnum;
import com.folib.components.thirdparty.foeyes.enums.EndpointsEnum;
import com.folib.components.thirdparty.foeyes.enums.UploadStatusEnum;
import com.folib.components.thirdparty.foeyes.reponse.ProjectInfo;
import com.folib.components.thirdparty.foeyes.request.CreateProjectRequest;
import com.folib.components.thirdparty.foeyes.request.ParentRequest;
import com.folib.constant.GlobalConstants;
import com.folib.domain.FoEyesConfig;
import com.folib.domain.bom.Bom;
import com.folib.domain.bom.FoEyes;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.users.security.SecurityTokenProvider;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.jose4j.jwt.JwtClaims;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.io.File;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.StandardOpenOption;
import java.util.*;

/**
 * @author veadan
 **/
@Slf4j
@Component
public class FoEyesComponent {

    @Value("${folib.temp}")
    private String tempPath;

    @Autowired
    private FoEyesProperties foEyesProperties;

    @Autowired
    private SecurityTokenProvider securityTokenProvider;

    @Autowired
    private ThirdPartyClientComponent thirdPartyClientComponent;

    @Autowired
    private DistributedCacheComponent distributedCacheComponent;

    public String getAccessToken() {
        String accessToken = "", foEyesAccessTokenCacheKey = "foEyesAccessTokenCacheKey";
        try {
            accessToken = distributedCacheComponent.get(foEyesAccessTokenCacheKey);
            if (StringUtils.isBlank(accessToken)) {
                accessToken = userLogin();
                distributedCacheComponent.put(foEyesAccessTokenCacheKey, accessToken);
                return accessToken;
            }
            JwtClaims jwtClaims = securityTokenProvider.getClaims(accessToken);
            long expirationTime = jwtClaims.getExpirationTime().getValueInMillis();
            long diff = expirationTime - System.currentTimeMillis();
            long maxDiff = 14400000;
            if (diff <= maxDiff) {
                accessToken = userLogin();
                distributedCacheComponent.put(foEyesAccessTokenCacheKey, accessToken);
                return accessToken;
            }
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
        return accessToken;
    }

    /**
     * 用户登录
     *
     * @return accessToken
     */
    public String userLogin() {
        String path = EndpointsEnum.USER_LOGIN.getPath();
        String url = getUrl(path);
        log.info("FoEyes user login url [{}]", url);
        Map<String, Object> formMap = Maps.newHashMap();
        formMap.put("username", foEyesProperties.getUsername());
        formMap.put("password", foEyesProperties.getPassword());
        String response = thirdPartyClientComponent.doRequest(Method.POST, url, null, (request) -> {
            request.form(formMap);
        }, String.class);
        log.info("FoEyes user login url [{}] response [{}]", url, response);
        return response;
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
        ServerResponse serverResponse = thirdPartyClientComponent.doCommonRequest(Method.PUT, url, headers, (request) -> {
            request.body(body);
        });
        if (HttpStatus.HTTP_CREATED != serverResponse.getCode()) {
            throw new RuntimeException(String.format("项目[%s]创建失败", createProjectRequest.getName()));
        }
        projectInfo = JSONObject.parseObject(serverResponse.getData(), ProjectInfo.class);
        log.info("FoEyes create project url [{}] params [{}] response [{}]", url, body, projectInfo);
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
        Map<String, String> headers = commonsHeaders();
        Map<String, String> params = Maps.newHashMap();
        params.put("name", name);
        params.put("version", "");
        url = url + createLinkStringByGet(params);
        log.info("FoEyes query project url [{}] params [{}] ", url, params);
        ServerResponse serverResponse = thirdPartyClientComponent.doCommonRequest(Method.GET, url, headers, null);
        if (HttpStatus.HTTP_NOT_FOUND == serverResponse.getCode()) {
            log.info("FoEyes query project url [{}] params [{}] response [{}]", url, params, serverResponse);
            return null;
        }
        if (HttpStatus.HTTP_OK != serverResponse.getCode()) {
            throw new RuntimeException(String.format("查询[%s]项目信息失败", name));
        }
        ProjectInfo projectInfo = JSONObject.parseObject(serverResponse.getData(), ProjectInfo.class);
        log.info("FoEyes query project url [{}] params [{}] response [{}]", url, params, projectInfo);
        return projectInfo;
    }

    /**
     * bom上传
     *
     * @param repositoryPath    制品信息
     * @param bomRepositoryPath bom信息
     * @throws Exception 异常
     */
    public void bomUpload(RepositoryPath repositoryPath, RepositoryPath bomRepositoryPath) throws Exception {
        bomUpload(repositoryPath, bomRepositoryPath, null);
    }

    /**
     * bom上传
     *
     * @param repositoryPath    制品信息
     * @param bomRepositoryPath bom信息
     * @param bomInfo           bom信息
     * @throws Exception 异常
     */
    public void bomUpload(RepositoryPath repositoryPath, RepositoryPath bomRepositoryPath, Bom bomInfo) throws Exception {
        String parentProjectName = String.format("%s/%s", repositoryPath.getStorageId(), repositoryPath.getRepositoryId());
        ProjectInfo parentProjectInfo = queryProject(parentProjectName);
        if (Objects.isNull(parentProjectInfo)) {
            throw new RuntimeException(String.format("repositoryPath [%s] 未找到对应的父项目信息", RepositoryFiles.relativizePath(repositoryPath)));
        }
        String name = parentProjectName + GlobalConstants.SEPARATOR + RepositoryFiles.relativizePath(repositoryPath);
        ProjectInfo projectInfo = createProject(CreateProjectRequest.builder().name(name).classifier(ClassifierEnum.FILE.getType()).parent(ParentRequest.builder().uuid(parentProjectInfo.getUuid()).build()).build());
        if (Objects.isNull(projectInfo)) {
            throw new RuntimeException(String.format("repositoryPath [%s] 未找到对应的项目信息", RepositoryFiles.relativizePath(repositoryPath)));
        }
        String path = EndpointsEnum.BOM_UPLOAD.getPath();
        String url = getUrl(path);
        Map<String, String> headers = commonsHeaders();
        Map<String, Object> formMap = Maps.newHashMap();
        formMap.put("project", projectInfo.getUuid());
        String parentPath = tempPath + File.separator + "foeyes" + File.separator + UUID.randomUUID();
        Bom bom = bomInfo;
        try {
            if (Objects.isNull(bom)) {
                String bomContent = Files.readString(bomRepositoryPath);
                bom = JSONObject.parseObject(bomContent, Bom.class);
            }
            if (Objects.isNull(bom.getFoEyes())) {
                bom.setFoEyes(FoEyes.builder().build());
            }
            String filePath = parentPath + File.separator + bomRepositoryPath.getFileName().toString();
            File tempFile = new File(filePath);
            FileUtil.writeUtf8String(bom.getBomValue().toJSONString(), tempFile);
            formMap.put("multiPart", FileUtil.file(tempFile));
            log.info("FoEyes bom upload url [{}]", url);
            String response = thirdPartyClientComponent.doRequest(Method.POST, url, headers, (request) -> {
                request.form(formMap);
            }, String.class);
            bom.getFoEyes().setUploadStatus(UploadStatusEnum.UPLOAD_SUCCESS.getType());
            log.info("FoEyes bom upload success url [{}] response [{}]", url, response);
        } catch (Exception ex) {
            if (Objects.nonNull(bom)) {
                bom.getFoEyes().setUploadStatus(UploadStatusEnum.UPLOAD_FAIL.getType());
            }
            log.error(ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException(ex.getMessage());
        } finally {
            FileUtil.del(new File(parentPath));
            if (Objects.nonNull(bom)) {
                Files.write(bomRepositoryPath, JSONObject.toJSONString(bom).getBytes(StandardCharsets.UTF_8), StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.CREATE);
            }
        }
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
            log.error("字符拼接错误：{}", ExceptionUtils.getStackTrace(ex));
        }
        return preStr;
    }
}
