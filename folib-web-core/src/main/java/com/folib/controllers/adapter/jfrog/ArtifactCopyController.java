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
package com.folib.controllers.adapter.jfrog;

import com.alibaba.fastjson.JSONObject;
import com.google.common.collect.Maps;
import com.folib.controllers.adapter.jfrog.dto.DockerCopyDto;
import com.folib.domain.ArtifactPromotion;
import com.folib.dto.TargetRepositoyDto;
import com.folib.providers.io.RepositoryPath;
import com.folib.services.ArtifactPromotionService;
import com.folib.services.DirectoryListingService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.compress.utils.Lists;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.inject.Inject;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * @author veadan
 * 需要验证仓库的唯一性
 */
@Slf4j
@RequestMapping("/artifactory")
@RestController
//@PreAuthorize("hasAuthority('ADMIN')")
@Api(description = "JFrog拷贝", tags = "JFrog拷贝")
public class ArtifactCopyController extends JFrogBaseController {

    @Inject
    private ArtifactPromotionService artifactPromotionService;

    @Inject
    @Qualifier("browseRepositoryDirectoryListingService")
    private volatile DirectoryListingService directoryListingService;

    /**
     * exampleUrl /api/copy/libs-release-local/org/acme?to=/ext-releases-local/org/acme-new&dry=1
     *
     * @param repositoryId
     * @param artifactPath
     * @param to
     * @return
     */
    @ApiOperation(value = "JFrog拷贝")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PostMapping("/api/copy/{repositoryId}/{artifactPath:.+}")
    public ResponseEntity<Object> copy(@PathVariable("repositoryId") String repositoryId,
                                       @PathVariable("artifactPath") String artifactPath,
                                       String to,
                                       String dry) throws Exception {
        String storageId = getDefaultStorageId(repositoryId);
        boolean checkRepository = checkRepository(storageId, repositoryId);
        if (!checkRepository) {
            return repositoryNotFound("source");
        }
        Map<String, Object> result = Maps.newHashMap();
        List<JSONObject> infoList = Lists.newArrayList();
        JSONObject jsonObject = new JSONObject();
        try {
            log.info("Copy repositoryId [{}] artifactPath [{}] to [{}] dry [{}]", repositoryId, artifactPath, to, dry);
            // 解析目标地址 目录地址必须是/开始
            if (!to.startsWith("/")) {
                to = "/" + to;
            }
            String[] targetArr = to.split("/");
            String targetRepositoryId = targetArr[1];
            String targetStorageId = getDefaultStorageId(targetRepositoryId);
            checkRepository = checkRepository(targetStorageId, targetRepositoryId);
            if (!checkRepository) {
                return repositoryNotFound("target");
            }
            ArtifactPromotion artifactPromotion = new ArtifactPromotion();
            artifactPromotion.setPath(artifactPath);
            artifactPromotion.setSrcStorageId(storageId);
            artifactPromotion.setSrcRepositoryId(repositoryId);
            List<TargetRepositoyDto> list = Lists.newArrayList();
            TargetRepositoyDto targetRepositoyDto = new TargetRepositoyDto();
            targetRepositoyDto.setTargetStorageId(targetStorageId);
            targetRepositoyDto.setTargetRepositoryId(targetRepositoryId);
            list.add(targetRepositoyDto);
            artifactPromotion.setTargetRepositoyList(list);
            ResponseEntity responseEntity = artifactPromotionService.copy(artifactPromotion);
            if (responseEntity.getStatusCode().value() == HttpStatus.OK.value()) {
                jsonObject.put("level", "info");
                jsonObject.put("message", "copying " + storageId + "/" + repositoryId + "/" + artifactPath + " to " + to + " completed successfully");
                infoList.add(jsonObject);
                result.put("messages", infoList);
                return ResponseEntity.ok(result);
            } else {
                jsonObject.put("level", "error");
                jsonObject.put("message", "copying " + storageId + "/" + repositoryId + "/" + artifactPath + " to " + to + " fail " + responseEntity.getStatusCode());
                infoList.add(jsonObject);
                result.put("messages", infoList);
                return ResponseEntity.ok(result);
            }
        } catch (Exception exception) {
            jsonObject.put("level", "error");
            jsonObject.put("message", "copying " + storageId + "/" + repositoryId + "/" + artifactPath + " to " + to + " fail " + exception.getMessage());
            infoList.add(jsonObject);
            result.put("messages", infoList);
            return ResponseEntity.ok(result);
        }

    }

    /**
     * exampleUrl POST api/docker/public-project/docker-local/v2/promote
     * {
     * "targetRepo": "docker-prod",
     * "dockerRepository": "jfrog/ubuntu"
     * }
     *
     * @param repositoryId
     * @param dockerCopyDto
     * @return
     */
    @ApiOperation(value = "JFrog镜像拷贝")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PostMapping("/api/docker/{repositoryId}/v2/promote")
    public ResponseEntity<Object> dockerCopy(@PathVariable("repositoryId") String repositoryId, @RequestBody DockerCopyDto dockerCopyDto) {
        log.info("Docker copy repositoryId [{}] params [{}]", repositoryId, JSONObject.toJSONString(dockerCopyDto));
        String storageId = getDefaultStorageId(repositoryId);
        boolean checkRepository = checkRepository(storageId, repositoryId);
        if (!checkRepository) {
            return repositoryNotFound("source");
        }
        String imageTag = dockerCopyDto.getTag();
        String artifactPath = dockerCopyDto.getDockerRepository();
        String split = "/";
        if (StringUtils.isNotBlank(imageTag)) {
            artifactPath = artifactPath + split + StringUtils.removeStart(imageTag, split);
        }
        String targetRepositoryId = dockerCopyDto.getTargetRepo();
        String targetStorageId = getDefaultStorageId(targetRepositoryId);
        checkRepository = checkRepository(targetStorageId, targetRepositoryId);
        if (!checkRepository) {
            return repositoryNotFound("target");
        }
        List<JSONObject> infoList = Lists.newArrayList();
        JSONObject jsonObject = new JSONObject();
        try {
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
            if (!Files.exists(repositoryPath) || !Files.isDirectory(repositoryPath)) {
                return artifactNotFound(artifactPath);
            }
            ArtifactPromotion artifactPromotion = new ArtifactPromotion();
            artifactPromotion.setPath(artifactPath);
            artifactPromotion.setSrcStorageId(storageId);
            artifactPromotion.setSrcRepositoryId(repositoryId);
            List<TargetRepositoyDto> list = new ArrayList<>();
            TargetRepositoyDto targetRepositoyDto = new TargetRepositoyDto();
            targetRepositoyDto.setTargetStorageId(targetStorageId);
            targetRepositoyDto.setTargetRepositoryId(targetRepositoryId);
            list.add(targetRepositoyDto);
            artifactPromotion.setTargetRepositoyList(list);
            ResponseEntity responseEntity = artifactPromotionService.copy(artifactPromotion);
            if (responseEntity.getStatusCode().value() == HttpStatus.OK.value()) {
                jsonObject.put("level", "info");
                jsonObject.put("message", "copying " + storageId + "/" + repositoryId + "/" + artifactPath + " to " + JSONObject.toJSONString(artifactPromotion) + " completed successfully");
                infoList.add(jsonObject);
            } else {
                jsonObject.put("level", "error");
                jsonObject.put("message", "copying " + storageId + "/" + repositoryId + "/" + artifactPath + " to " + JSONObject.toJSONString(artifactPromotion) + " fail " + responseEntity.getStatusCode());
                infoList.add(jsonObject);
            }
        } catch (Exception exception) {
            jsonObject.put("level", "error");
            jsonObject.put("message", "copying " + storageId + "/" + repositoryId + "/" + artifactPath + " to " + JSONObject.toJSONString(dockerCopyDto) + " fail " + exception.getMessage());
            infoList.add(jsonObject);
        }
        return ResponseEntity.ok(infoList);
    }
}
