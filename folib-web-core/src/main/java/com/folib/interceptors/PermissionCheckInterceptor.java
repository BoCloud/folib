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
package com.folib.interceptors;

import cn.hutool.core.io.FileUtil;
import cn.hutool.extra.spring.SpringUtil;
import cn.hutool.json.JSONUtil;
import com.alibaba.fastjson.JSONObject;
import com.google.common.collect.Lists;
import com.folib.components.artifact.ArtifactComponent;
import com.folib.components.auth.AuthComponent;
import com.folib.config.PermissionCheck;
import com.folib.controllers.support.ErrorResponseEntityBody;
import com.folib.domain.ArtifactParse;
import com.folib.scanner.common.util.IPUtil;
import com.folib.security.vote.ExtendedAuthoritiesVoter;
import com.folib.wrapper.RequestWrapper;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.SerializationUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.method.HandlerMethod;
import org.springframework.web.servlet.HandlerInterceptor;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.util.*;

/**
 * 自定义权限拦截器 校验节点间请求的白名单
 *
 * @author veadan
 */
@Slf4j
public class PermissionCheckInterceptor implements HandlerInterceptor {
    private final Set<String> currentWhiteList = new HashSet<>();


    public boolean preHandle(HttpServletRequest request, HttpServletResponse response,
                             Object handler) throws Exception {
        // 仅处理 Controller 方法，跳过静态资源请求
        if (!(handler instanceof HandlerMethod)) {
            return true;
        }
        HandlerMethod handlerMethod = (HandlerMethod) handler;
        PermissionCheck permission = findPermissionCheck(handlerMethod);

        //如果没有添加权限注解则直接跳过允许访问
        if (permission == null) {
            return true;
        }
        //获取注解中的值
        String resourceKey = permission.resourceKey();

        //是否在白名单中
        String ipAddr = IPUtil.getIpAddr(request);
        log.info("Current request ip [{}]", ipAddr);
        //获取用的角色权限列表中是否拥有该权限
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (Objects.isNull(authentication) || !authentication.isAuthenticated()) {
            handlerResponse(response);
            return false;
        }
        String storageKey = permission.storageKey();
        String repositoryKey = permission.repositoryKey();
        String storageId = "", repositoryId = "";
        Collection<? extends GrantedAuthority> globalAuthorities = authentication.getAuthorities();
        List<GrantedAuthority> authorities = Lists.newArrayList();
        globalAuthorities.forEach(item -> {
            authorities.add(SerializationUtils.clone(item));
        });
        if (StringUtils.isNotBlank(storageKey) && StringUtils.isNotBlank(repositoryKey)) {
            storageId = request.getParameter(storageKey);
            repositoryId = request.getParameter(repositoryKey);
            ExtendedAuthoritiesVoter extendedAuthoritiesVoter = SpringUtil.getBean(ExtendedAuthoritiesVoter.class);
            boolean result = extendedAuthoritiesVoter.handlerRestrictedRepository(authorities, storageId, repositoryId);
            if (result) {
                Authentication newAuthentication = new UsernamePasswordAuthenticationToken(
                        authentication.getPrincipal(),
                        authentication.getCredentials(),
                        authorities
                );
                SecurityContextHolder.getContext().setAuthentication(newAuthentication);
            }
        }
        boolean auth = authorities.stream().anyMatch(item -> item.getAuthority().equals(resourceKey));
        if (auth) {
            return true;
        }
        String pathKey = permission.pathKey();
        if (StringUtils.isNotBlank(storageKey) && StringUtils.isNotBlank(repositoryKey)) {
            String filePathMap = request.getParameter("filePathMap");
            List<String> filePaths = null;
            if (StringUtils.isNotBlank(filePathMap)) {
                JSONObject filePathJson = JSONObject.parseObject(request.getParameter("filePathMap"));
                Iterator<String> iterable = filePathJson.keySet().iterator();
                String filePathKey = "", filePath;
                filePaths = Lists.newArrayList();
                while (iterable.hasNext()) {
                    filePathKey = iterable.next();
                    filePath = filePathJson.getString(filePathKey);
                    if (StringUtils.isNotBlank(filePath)) {
                        filePaths.add(filePath);
                    }
                }
            }
            if (StringUtils.isNotBlank(pathKey)) {
                String path = request.getParameter(pathKey);
                if (StringUtils.isNotBlank(path)) {
                    filePaths = Lists.newArrayList(path);
                }
            }
            String parseArtifact = request.getParameter("parseArtifact");
            if (StringUtils.isNotBlank(parseArtifact) && JSONUtil.isJson(parseArtifact)) {
                ArtifactParse artifactParse = null;
                artifactParse = JSONObject.parseObject(parseArtifact, ArtifactParse.class);
                ArtifactComponent artifactComponent = SpringUtil.getBean(ArtifactComponent.class);
                String artifactPath = artifactComponent.calcMavenArtifactPath(storageId, repositoryId, artifactParse.getGroupId(), artifactParse.getArtifactId(), artifactParse.getVersion(), FileUtil.getName(artifactParse.getFilePath()));
                if (StringUtils.isNotBlank(artifactPath)) {
                    filePaths = Lists.newArrayList();
                    filePaths.add(artifactPath);
                }
            }
            String body = StringUtils.EMPTY;
            if (request instanceof RequestWrapper) {
                body = ((RequestWrapper) request).getBody();
            }
            if (StringUtils.isNotBlank(body)) {
                JSONObject jsonObject = JSONObject.parseObject(body);
                storageId = jsonObject.getString(storageKey);
                repositoryId = jsonObject.getString(repositoryKey);
            }
            AuthComponent authComponent = SpringUtil.getBean(AuthComponent.class);
            Set<String> privileges = authComponent.getAllPrivileges(storageId, repositoryId, filePaths);
            boolean flag = privileges.contains(resourceKey);
            if (!flag) {
                handlerResponse(response);
            }
            return flag;
        }
        handlerResponse(response);
        return false;
    }

    /**
     * 根据handlerMethod返回注解信息
     *
     * @param handlerMethod 方法对象
     * @return PermissionCheck注解
     */
    private PermissionCheck findPermissionCheck(HandlerMethod handlerMethod) {
        //在方法上寻找注解
        PermissionCheck permission = handlerMethod.getMethodAnnotation(PermissionCheck.class);
        if (permission == null) {
            //在类上寻找注解
            permission = handlerMethod.getBeanType().getAnnotation(PermissionCheck.class);
        }
        return permission;
    }

    private void handlerResponse(HttpServletResponse response) {
        try {
            response.setContentType(org.springframework.http.MediaType.APPLICATION_JSON_VALUE);
            response.setStatus(HttpServletResponse.SC_FORBIDDEN);
            String msg = "Access to the requested resource is not authorized.";
            response.getWriter().println(JSONObject.toJSONString(new ErrorResponseEntityBody(msg)));
            response.flushBuffer();
        } catch (Exception ex) {
            log.error("处理响应失败：{}", ExceptionUtils.getStackTrace(ex));
        }
    }
}
