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
package com.folib.components;

import com.alibaba.fastjson.JSONObject;
import com.google.common.collect.Maps;
import com.folib.artifact.coordinates.DockerCoordinates;
import com.folib.constant.GlobalConstants;
import com.folib.domain.AuthInfo;
import com.folib.domain.client.ResponseResult;
import com.folib.enums.ResponseDataTypeEnum;
import com.folib.storage.repository.remote.RemoteRepository;
import com.folib.util.CommonUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import javax.ws.rs.core.MultivaluedHashMap;
import javax.ws.rs.core.MultivaluedMap;
import java.util.Collections;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author veadan
 * @date 2024/1/29
 **/
@Component
public class DockerAuthComponent {

    @Inject
    private DockerClientComponent dockerClientComponent;

    @Inject
    private DistributedCacheComponent distributedCacheComponent;

    public void handleAuthToken(RemoteRepository remoteRepository, String storageId, String repositoryId, String imagePath, MultivaluedMap<String, Object> requestHeaders) {
        if (Objects.isNull(remoteRepository)) {
            return;
        }
        String end = "/v2/", remoteUrl = remoteRepository.getUrl();
        String authToken = getAuthToken(storageId, repositoryId, remoteUrl, imagePath);
        if (StringUtils.isNotBlank(authToken)) {
            requestHeaders.put("Authorization", Collections.singletonList(authToken));
            return;
        }
        if (!remoteUrl.endsWith(GlobalConstants.SEPARATOR)) {
            remoteUrl = remoteUrl.concat(GlobalConstants.SEPARATOR);
        }
        int index = remoteUrl.indexOf(end) + end.length();
        String targetUrl = remoteUrl.substring(0, index);
        ResponseResult responseResult = dockerClientComponent.doGet(storageId, repositoryId, targetUrl);
        if (Objects.isNull(responseResult)) {
            return;
        }
        MultivaluedMap<String, String> headers = responseResult.getHeaders();
        if (MapUtils.isEmpty(headers)) {
            return;
        }
        String authenticate = headers.getFirst("WWW-Authenticate");
        if (StringUtils.isBlank(authenticate)) {
            return;
        }
        String pattern = "realm=\"(.*?)\"";
        String authUrl = resolveAuthParam(authenticate, pattern);
        if (StringUtils.isBlank(authUrl)) {
            return;
        }
        pattern = "service=\"(.*?)\"";
        String service = resolveAuthParam(authenticate, pattern);
        if (StringUtils.isBlank(service)) {
            return;
        }
        String scope = "repository:%s%s:pull";
        String scopeRepository = "library/";
        if (imagePath.split(GlobalConstants.SEPARATOR).length > 1) {
            scopeRepository = "";
        }
        scope = String.format(scope, scopeRepository, StringUtils.removeEnd(imagePath, GlobalConstants.SEPARATOR));
        Map<String, String> params = Maps.newHashMap();
        params.put("scope", scope);
        params.put("service", service);
        authUrl = authUrl + CommonUtils.createLinkStringByGet(params);
        responseResult = dockerClientComponent.doGet(storageId, repositoryId, authUrl);
        if (StringUtils.isBlank(responseResult.getData()) || !ResponseDataTypeEnum.JSON.getType().equals(responseResult.getDataType().getType())) {
            return;
        }
        AuthInfo authInfo = JSONObject.parseObject(responseResult.getData(), AuthInfo.class);
        if (StringUtils.isBlank(authInfo.getToken())) {
            return;
        }
        if (MapUtils.isEmpty(requestHeaders)) {
            requestHeaders = new MultivaluedHashMap();
        }
        requestHeaders.put("Authorization", Collections.singletonList(String.format("%s %s", "Bearer", authInfo.getToken())));
        String key = String.format("docker-auth-%s-%s-%s-%s", storageId, repositoryId, remoteUrl, imagePath);
        long ttl = 120;
        if (Objects.nonNull(authInfo.getExpiresIn()) && authInfo.getExpiresIn() < ttl) {
            ttl = authInfo.getExpiresIn() - 30;
        }
        if (ttl > 0 && DockerCoordinates.DOCKER_LAYER_DIR_NAME_LIST.stream().noneMatch(item -> item.equals(imagePath))) {
            distributedCacheComponent.put(key, String.format("%s %s", "Bearer", authInfo.getToken()), ttl, TimeUnit.SECONDS);
        }
    }

    private String resolveAuthParam(String authenticate, String pattern) {
        Pattern r = Pattern.compile(pattern);
        Matcher m = r.matcher(authenticate);
        if (m.find()) {
            return m.group(1);
        } else {
            return "";
        }
    }

    private String getAuthToken(String storageId, String repositoryId, String remoteUrl, String imagePath) {
        String key = String.format("docker-auth-%s-%s-%s-%s", storageId, repositoryId, remoteUrl, imagePath);
        return distributedCacheComponent.get(key);
    }

}
